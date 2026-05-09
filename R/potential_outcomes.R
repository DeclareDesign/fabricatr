#' Build potential outcomes columns
#'
#' For each combination of condition values, evaluates the RHS of \code{x}
#' with the assignment variable(s) set to those values, producing one column
#' per condition. The column names follow the pattern
#' \code{<outcome>_<var>_<value>}.
#'
#' When called inside \code{fabricate()} without a left-hand name (i.e. as an
#' unnamed argument), the resulting columns are appended automatically.
#'
#' @param x A two-sided formula. LHS: outcome variable name. RHS: expression
#'   for the potential outcome (may reference the assignment variable and other
#'   columns).
#' @param conditions Named list mapping assignment variable name(s) to their
#'   possible values. Default \code{list(Z = c(0, 1))}.
#' @param sep Column-name separator. Default \code{"_"}.
#'
#' @return A data frame with one column per condition combination, to be
#'   appended by \code{fabricate()}.
#'
#' @examples
#' fabricate(
#'   N = 10, U = rnorm(N),
#'   potential_outcomes(Y ~ 0.5 * Z + U)
#' )
#'
#' # Three conditions
#' fabricate(
#'   N = 10, U = rnorm(N),
#'   potential_outcomes(Y ~ Z * 0.5 + U, conditions = list(Z = 0:2))
#' )
#'
#' # Multi-arm factorial
#' fabricate(
#'   N = 10, U = rnorm(N),
#'   potential_outcomes(Y ~ 0.3 * Z1 + 0.5 * Z2 + U,
#'                      conditions = list(Z1 = 0:1, Z2 = 0:1))
#' )
#'
#' @importFrom rlang f_lhs eval_tidy as_quosure
#' @export
potential_outcomes <- function(x, conditions = list(Z = c(0, 1)), sep = "_") {
  outcome <- rlang::f_lhs(x)
  if (is.null(outcome)) {
    stop("Provide the outcome name on the LHS of the formula (e.g. Y ~ ...).")
  }

  conds <- expand.grid(conditions, stringsAsFactors = FALSE)
  out  <- list()
  rhs  <- rlang::f_rhs(x)
  fenv <- environment(x)

  for (i in seq_len(nrow(conds))) {
    row <- conds[i, , drop = FALSE]
    nm  <- paste0(outcome, sep,
                  paste(names(row), unlist(row), sep = sep, collapse = sep))
    # Create a child env so condition injections don't pollute fenv across loops
    child <- new.env(parent = fenv)
    list2env(as.list(row), envir = child)
    out[[nm]] <- eval(rhs, envir = child, enclos = fenv)
  }

  as.data.frame(out)
}

#' Reveal observed outcomes from potential outcomes columns
#'
#' Implements the switching equation: for each unit, selects the potential
#' outcome column corresponding to that unit's realized assignment.
#'
#' @param x A formula of the form \code{outcome ~ assignment} or
#'   \code{outcome ~ Z1 + Z2} for factorial assignments. The outcome name and
#'   assignment variable(s) must match columns already in the data (typically
#'   created by \code{potential_outcomes}).
#'
#' @return Numeric vector of revealed outcomes.
#'
#' @examples
#' dat <- fabricate(
#'   N = 10, U = rnorm(N),
#'   potential_outcomes(Y ~ 0.5 * Z + U)
#' )
#' fabricate(
#'   data = dat,
#'   Z = rbinom(N, 1, 0.5),
#'   Y = reveal_outcomes(Y ~ Z)
#' )
#'
#' @importFrom rlang eval_tidy f_lhs as_name
#' @importFrom stats terms
#' @export
reveal_outcomes <- function(x) {
  outcome  <- rlang::as_name(rlang::f_lhs(x))
  z_vars   <- labels(stats::terms(x))

  # Build a data frame of realized assignment values from the calling env
  assign_expr <- str2lang(
    paste0("data.frame(", paste(z_vars, collapse = ", "), ")")
  )
  assign_df <- rlang::eval_tidy(assign_expr, env = environment(x))

  # Construct the potential outcome column name for each unit
  po_cols <- do.call(paste, c(
    list(outcome),
    mapply(paste, z_vars, assign_df, sep = "_", SIMPLIFY = FALSE),
    sep = "_"
  ))

  unique_cols <- unique(po_cols)
  po_expr <- str2lang(
    paste0("data.frame(", paste(unique_cols, collapse = ", "), ")")
  )
  po_df <- rlang::eval_tidy(po_expr, env = environment(x))

  row_idx <- seq_len(nrow(po_df))
  col_idx <- match(po_cols, colnames(po_df))
  as.data.frame(po_df)[cbind(row_idx, col_idx)]
}

# str2lang backport for R < 3.6 (not needed for R >= 4.1 per DESCRIPTION,
# kept for clarity)
if (!exists("str2lang")) {
  str2lang <- function(s) parse(text = s, keep.source = FALSE)[[1L]]
}

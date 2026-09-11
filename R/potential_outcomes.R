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
#' @return A data frame with one column per combination of the values in
#'   \code{conditions}, named \code{<outcome><sep><var><sep><value>} and, with
#'   more than one assignment variable, carrying one such pair per variable:
#'   \code{Y_Z1_0_Z2_0}, \code{Y_Z1_1_Z2_0}, \code{Y_Z1_0_Z2_1},
#'   \code{Y_Z1_1_Z2_1}, the first variable varying fastest. Called inside
#'   \code{fabricate()} without a name, these columns are appended to the frame.
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
#' @return A vector with one element per row, holding each unit's outcome
#'   under the assignment it actually received, and carrying the type of the
#'   potential outcome columns it reads. A factor reveals as a factor, keeping
#'   its levels and their order, and a \code{Date} as a \code{Date}. Where the
#'   conditions do not agree on one set of factor levels there are none to
#'   keep, so the result is \code{character} and a warning says so.
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

  col_idx <- match(po_cols, colnames(po_df))
  reveal_one_per_row(po_df, col_idx, outcome)
}

# Take one value per row across the potential outcome columns, keeping the type
# those columns carry.
#
# Matrix-indexing the data frame is shorter, and is what 1.0.2 and every 2.0
# build before this one did, but it goes through `as.matrix()`, which renders a
# factor as character and a Date as a string. An ordered outcome came back with
# its levels gone, so re-factoring it put them in alphabetical order: a Likert
# outcome declared `lo < mid < hi` revealed as `hi < lo < mid`, and any model
# fitted on it used the wrong baseline and the wrong ordering without saying so.
reveal_one_per_row <- function(po_df, col_idx, outcome) {
  if (any(vapply(po_df, is.factor, logical(1L)))) {
    po_df <- reconcile_factor_levels(po_df, outcome)
  }

  # `po_df` is built from these very column names, so every row matches one of
  # them and the first column is only ever a carrier of class and levels.
  out <- po_df[[1L]]
  for (j in seq_along(po_df)) {
    take <- which(col_idx == j)
    if (length(take)) out[take] <- po_df[[j]][take]
  }
  out
}

# The revealed vector can carry only one set of factor levels. Where every
# condition already agrees on them, they are kept, ordered or not. Where they
# do not agree, the alternative is to invent an order across level sets that
# were never meant to be compared, so those fall back to character and say so.
reconcile_factor_levels <- function(po_df, outcome) {
  all_factors <- all(vapply(po_df, is.factor, logical(1L)))
  same_levels <- length(unique(lapply(po_df, levels))) == 1L
  same_order <- length(unique(vapply(po_df, is.ordered, logical(1L)))) == 1L

  if (all_factors && same_levels && same_order) return(po_df)

  warning("The potential outcomes of `", outcome, "` do not share one set of ",
          "factor levels, so the revealed outcome is character. Give every ",
          "condition the same `levels` to keep them.", call. = FALSE)
  lapply(po_df, as.character)
}

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
#' @param sep Column-name separator. Default \code{"_"}. Give
#'   \code{reveal_outcomes()} the same \code{sep}.
#'
#' @return A data frame with one column per combination of the values in
#'   \code{conditions}, named \code{<outcome><sep><var><sep><value>} and, with
#'   more than one assignment variable, carrying one such pair per variable:
#'   \code{Y_Z1_0_Z2_0}, \code{Y_Z1_1_Z2_0}, \code{Y_Z1_0_Z2_1},
#'   \code{Y_Z1_1_Z2_1}, the first variable varying fastest. The names are kept
#'   as written even where they are not syntactic R names, so a negative
#'   condition gives \code{Y_Z_-1} and a condition \code{"b c"} gives
#'   \code{Y_Z_b c}; refer to such a column with backticks or \code{.data}.
#'   Called inside \code{fabricate()} without a name, these columns are
#'   appended to the frame.
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

  # `check.names = TRUE` would turn `Y_Z_-1` into `Y_Z_.1` and `Y_Z_b c` into
  # `Y_Z_b.c`, names `reveal_outcomes()` cannot rebuild from the assignment.
  as.data.frame(out, check.names = FALSE)
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
#' @param sep Column-name separator. Default \code{"_"}. It must match the
#'   \code{sep} given to \code{potential_outcomes()}.
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
#' # A separator other than "_" is given to both
#' fabricate(
#'   N = 10, U = rnorm(N),
#'   potential_outcomes(Y ~ 0.5 * Z + U, sep = "."),
#'   Z = rbinom(N, 1, 0.5),
#'   Y = reveal_outcomes(Y ~ Z, sep = ".")
#' )
#'
#' @importFrom rlang eval_tidy f_lhs as_name
#' @importFrom stats terms
#' @export
reveal_outcomes <- function(x, sep = "_") {
  outcome <- rlang::as_name(rlang::f_lhs(x))
  z_vars <- assignment_names(x)
  env <- environment(x)

  # Every column is read by name from the environment the formula was written
  # in, which inside `fabricate()` is the data mask. Pasting the names into
  # code to evaluate, as 1.0.2 did, parsed `Y_Z_-1` as a subtraction and
  # failed on `Y_Z_b c` as a syntax error.
  read_column <- function(nm) rlang::eval_tidy(rlang::sym(nm), env = env)

  assign_df <- z_vars |>
    rlang::set_names() |>
    lapply(read_column) |>
    as.data.frame(check.names = FALSE)

  # Construct the potential outcome column name for each unit
  po_cols <- do.call(paste, c(
    list(outcome),
    mapply(paste, z_vars, assign_df, sep = sep,
           SIMPLIFY = FALSE, USE.NAMES = FALSE),
    sep = sep
  ))

  # Inside a data mask (`fabricate()` or dplyr) the potential outcomes are read
  # through `.data`, which looks only at the columns, so a global object that
  # happens to be called `Y_Z_2` cannot stand in for a missing column.
  pronoun <- get0(".data", envir = env)
  read_po <- if (inherits(pronoun, "rlang_data_pronoun")) {
    function(nm) tryCatch(pronoun[[nm]], error = function(e) NULL)
  } else {
    function(nm) get0(nm, envir = env)
  }

  unique_cols <- unique(po_cols)
  po_list <- lapply(rlang::set_names(unique_cols), read_po)
  found <- !vapply(po_list, is.null, logical(1L))
  if (!all(found)) {
    stop("reveal_outcomes() can't find the potential outcome column ",
         paste0("`", unique_cols[!found], "`", collapse = ", "), ".\n",
         "  Check that every assigned value is among the `conditions` given ",
         "to potential_outcomes(), and that `sep` matches the one used ",
         "there (here \"", sep, "\").", call. = FALSE)
  }

  po_df <- as.data.frame(po_list, check.names = FALSE)

  col_idx <- match(po_cols, unique_cols)
  reveal_one_per_row(po_df, col_idx, outcome)
}

# The names of the assignment variables on the right-hand side of a
# `reveal_outcomes()` formula, without the backticks `labels(terms(x))` keeps
# on a non-syntactic one. Each must be a bare name, since it is half of a
# column name: `Y ~ log(Z)` names no potential outcome column.
assignment_names <- function(x) {
  tt <- stats::terms(x)
  vars <- as.list(attr(tt, "variables"))[-1L]
  if (attr(tt, "response") == 1L) vars <- vars[-1L]
  if (!length(vars) || !all(vapply(vars, is.symbol, logical(1L)))) {
    stop("reveal_outcomes() needs the assignment variables named on the ",
         "right-hand side, as in `Y ~ Z` or `Y ~ Z1 + Z2`.", call. = FALSE)
  }
  vapply(vars, rlang::as_string, character(1L))
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

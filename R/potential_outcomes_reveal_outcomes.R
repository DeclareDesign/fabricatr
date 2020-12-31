
#' Build potential outcomes variables
#'
#' Function to draw multiple potential outcomes, one for each condition that an assignment variable can be set to.
#'
#' @param x Formula describing the potential outcomes with the outcome name on the left hand side and the expression describing the potential outcomes on the right hand side, e.g. \code{Y ~ 0.1 * Z + rnorm(N)} (this would draw two potential outcomes columns by default, named Y_Z_0 and Y_Z_1).
#' @param conditions A list of conditions for each assignment variable. Defaults to \code{list(Z = c(0, 1))}.
#' @param sep Separator inserted between the outcome name and the assignment variable name used to construct the potential outcome variable names, defaults to "_".
#'
#' @examples
#'
#' fabricate(
#'   N = 10,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ 0.1 * Z + U)
#' )
#'
#' @importFrom rlang eval_tidy as_quosure f_lhs
#' @export
potential_outcomes <- function(x, conditions = list(Z = c(0, 1)), sep = "_") {
  conds <- expand.grid(conditions, stringsAsFactors = FALSE)
  pos <- list()
  for(i in 1:nrow(conds)) {
    cond <- conds[i, , drop = FALSE]
    nm <- paste0(f_lhs(x), sep, paste0(names(cond), sep, cond, collapse = sep))
    list2env(x = as.list(cond), environment(x))
    pos[[nm]] <- eval_tidy(as_quosure(x))
  }
  as.data.frame(pos)
}

#' Reveal outcomes
#'
#' Implements a generalized switching equation. Reveals observed outcomes from multiple potential outcomes variables and an assignment variable.
#'
#' @param x A formula with the outcome name on the left hand side and the assignment variable on the right hand side (e.g., \code{Y ~ Z}). The LHS can include multiple variables (e.g., \code{c(Y1, Y2) ~ Z}).
#'
#' @examples
#'
#' dat <- fabricate(
#'   N = 10,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ 0.1 * Z + U)
#' )
#'
#' dat %>%
#'   fabricate(
#'     Z = rbinom(N, 1, prob = 0.5),
#'     Y = reveal_outcomes(Y ~ Z)
#'   )
#'
#' @importFrom rlang eval_tidy f_lhs as_name
#' @export
reveal_outcomes <- function(x){

  # obtain character strings
  outcome_variables <- as_name(f_lhs(x))
  assignment_variables <- labels(terms(x))
  assignment_variables_expr <- str2lang(paste0("data.frame(", paste0(assignment_variables, collapse = ", "), ")"))

  # obtain assignment columns
  assignment_data <- eval_tidy(assignment_variables_expr, env = environment(x))

  # list of assignmentname_conditionvalue
  potential_cols <-
    mapply(paste,
           assignment_variables,
           assignment_data,
           sep = "_",
           SIMPLIFY = FALSE)

  potential_cols <- do.call(paste, c(outcome_variables, potential_cols, sep = "_"))

  upoc <- unique(potential_cols)

  # obtain potential outcomes columns
  po_variables_expr <- str2lang(paste0("data.frame(", paste0(upoc, collapse = ", "), ")"))
  po_data <- eval_tidy(po_variables_expr, env = environment(x))

  matching_rows <- seq_len(nrow(po_data))
  matching_cols <- match(potential_cols, colnames(po_data))

  as.data.frame(po_data)[cbind(matching_rows, matching_cols)]
}


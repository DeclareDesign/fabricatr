#' Reveal outcomes
#'
#' Implements a generalized switching equation. Reveals observed outcomes from multiple potential outcomes variables and an assignment variable.
#'
#' @param x A formula with the outcome name on the left hand side and assignment variables on the right hand side (e.g., \code{Y ~ Z}).
#'
#' @examples
#'
#' dat <- fabricate(
#'   N = 10,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ 0.1 * Z + U)
#' )
#'
#' fabricate(
#'   data = dat,
#'   Z = rbinom(N, 1, prob = 0.5),
#'   Y = reveal_outcomes(Y ~ Z)
#' )
#'
#' fabricate(
#'   N = 10,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ 0.1 * Z1 + 0.3 * Z2 + 0.5 * Z1 * Z2 + U,
#'                      conditions = list(Z1 = c(0, 1),
#'                                        Z2 = c(0, 1))),
#'   Z1 = rbinom(N, 1, prob = 0.5),
#'   Z2 = rbinom(N, 1, prob = 0.5),
#'   Y = reveal_outcomes(Y ~ Z1 + Z2)
#' )
#'
#'
#' @importFrom rlang eval_tidy f_lhs as_name
#' @importFrom stats terms
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


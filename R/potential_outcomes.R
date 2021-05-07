
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
#' # equivalently,
#'
#' fabricate(
#'   N = 10,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ 0.1 * Z + U,
#'                      conditions = list(Z = c(0, 1)))
#' )
#'
#' fabricate(
#'   N = 10,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ 0.1 * Z + U,
#'                      conditions = list(Z = c(1, 2, 3)))
#' )
#'
#' fabricate(
#'   N = 10,
#'   U = rnorm(N),
#'   potential_outcomes(Y ~ 0.1 * Z1 + 0.3 * Z2 + 0.5 * Z1 * Z2 + U,
#'                      conditions = list(Z1 = c(0, 1),
#'                                        Z2 = c(0, 1)))
#' )
#'
#'
#' @importFrom rlang eval_tidy as_quosure f_lhs
#' @export
potential_outcomes <- function(x, conditions = list(Z = c(0, 1)), sep = "_") {
  conds <- expand.grid(conditions, stringsAsFactors = FALSE)
  pos <- list()
  for(i in 1:nrow(conds)) {
    cond <- conds[i, , drop = FALSE]
    outcome <- f_lhs(x)
    if(is.null(outcome)) stop("Please provide an outcome name on the lefhand side of the formula for your potential outcomes, left of ~.")
    nm <- paste0(outcome, sep, paste0(names(cond), sep, cond, collapse = sep))
    list2env(x = as.list(cond), environment(x))
    pos[[nm]] <- eval_tidy(as_quosure(x))
  }
  data.frame(pos)
}

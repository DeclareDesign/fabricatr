
#' Draw multivariate random variables
#'
#' @param formula Formula describing the multivariate draw. The lefthand side is the names or prefix and the right-hand side is the multivariate draw function call, such as mvrnorm from the MASS library or rmnom from the extraDistr library.
#' @param sep Separator string between prefix and variable number. Only used when a single character string is provided and multiple variables created.
#'
#' @return tibble
#'
#' @export
#'
#' @examples
#'
#' library(MASS)
#'
#' # draw from multivariate normal distribution
#' dat <-
#'   draw_multivariate(c(Y_1, Y_2) ~ mvrnorm(
#'     n = 500,
#'     mu = c(0, 0),
#'     Sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)
#'   ))
#'
#'
#' cor(dat)
#'
#'
#' # equivalently, you can provide a prefix for the variable names
#' # (easier if you have many variables)
#' draw_multivariate(Y ~ mvrnorm(
#'   n = 5,
#'   mu = c(0, 0),
#'   Sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)
#' ))
#'
#' # within fabricate
#' fabricate(
#'   N = 100,
#'   draw_multivariate(c(Y_1, Y_2) ~ mvrnorm(
#'     n = N,
#'     mu = c(0, 0),
#'     Sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)
#'   ))
#' )
#'
#' # You can also write the following, which works but gives less control over the names
#' fabricate(N = 100,
#' Y = mvrnorm(
#'   n = N,
#'   mu = c(0, 0),
#'   Sigma = matrix(c(1, 0.5, 0.5, 1), 2, 2)
#' ))
#'
#' @importFrom rlang f_lhs f_rhs eval_tidy as_label call_args
draw_multivariate <- function(formula, sep = "_") {
  # draw from multivariate function that returns matrix
  mat <- eval_tidy(f_rhs(formula), env = environment(formula))

  # handle naming
  lhs <- f_lhs(formula)
  if(is.null(lhs)) stop("Please provide a way to name the variables on the lefthand side of the formula, either a prefix or a vector of names.")
  if(inherits(lhs, "name")) {
    nm <- paste0(lhs, sep, seq_len(ncol(mat)))
  } else if(inherits(lhs, "call")) {
    nm <- sapply(call_args(lhs), as_label)
    if(length(nm) != ncol(mat)){
      stop("You provided a different number of variable names (", paste0(nm, collapse = ", "), "). Either change the names or change your random variable specification.")
    }
  }
  colnames(mat) <- nm

  # return matrix converted to tibble
  data.frame(mat)
}


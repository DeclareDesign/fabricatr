
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
#' library(extraDistr)
#' # draw from multinomial (multivariate binomial) distribution
#' draw_multivariate(Y ~ rmnom(5, size = 3, prob = c(0.25, 0.1)))
#' draw_multivariate(c(Y_1, Y_2) ~ rmnom(5, size = 3, prob = c(0.25, 0.1)))
#'
#' @importFrom rlang f_lhs f_rhs eval_tidy as_label call_args
#' @importFrom tibble as_tibble
draw_multivariate <- function(formula, sep = "_") {
  # draw from multivariate function that returns matrix
  mat <- eval_tidy(f_rhs(formula), env = environment(formula))

  # handle naming
  lhs <- f_lhs(formula)
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
  as_tibble(mat)
}


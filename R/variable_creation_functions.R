

#' Binary variable from a binomial distribution with a logit link
#'
#' @param data vector, scalar, matrix, or data.frame representing the latent variable used to draw the binary outcome
#'
#' @export
#'
#' @examples
#'
#' fabricate_data(N = 10, Y1 = rnorm(N),  Y2 = binary_logit(Y1))
#'
#' binary_logit(runif(10))
#'
binary_logit <- binary_logistic_variable <- function(data) {
  rbinom(
    n = ifelse(is.vector(data), length(data), nrow(data)),
    size = 1,
    prob = 1 / (1 + exp(-data))
  )
}

#' Count variable from a binomial distribution with a logit link
#'
#' @param data vector, scalar, matrix, or data.frame representing the latent variable used to draw the count outcome
#'
#' @param k number of binomial trials, i.e. maximum of the count variable
#'
#' @export
#'
#' @examples
#'
#' fabricate_data(N = 10, Y1 = rnorm(N),  Y2 = binomial_count(Y1, 4))
#' fabricate_data(N = 10, Y1 = rnorm(N),  Y2 = binomial_count(Y1, 2))
#'
#' binomial_count(runif(50), 3)
binomial_count <- function(data, k) {
  rbinom(ifelse(is.vector(data), length(data), nrow(data)),
         size = k,
         prob = 1 / (1 + exp(-data)))
}

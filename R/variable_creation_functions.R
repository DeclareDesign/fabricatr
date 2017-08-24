#' Draw a binary variable from a binomial distribution
#'
#' Drawing binary variables based on probabilities or latent traits is a common task that can be cumbersome.
#'
#' @param vector vector representing either the latent variable used to draw the count outcome (if link is "logit" or "probit") or the probability for the count outcome (if link is "identity")
#' @param link link function between the latent variable and the probability of a postiive outcome, i.e. "logit", "probit", or "identity". For the "identity" link, the latent variable must be a probability.
#'
#' @importFrom stats pnorm
#'
#' @export
#'
#' @examples
#'
#' fabricate_data(N = 10, Y1 = rnorm(N),  Y2 = draw_binary(Y1))
#' fabricate_data(N = 10, Y1 = rnorm(N),  Y2 = draw_binary(Y1, link = "logit"))
#' fabricate_data(N = 10, Y1 = rnorm(N),  Y2 = draw_binary(Y1, link = "probit"))
#' fabricate_data(N = 10, Y1 = runif(N, 0, 1),  Y2 = draw_binary(Y1, link = "identity"))
#'
#' draw_binary(rnorm(10))
#' draw_binary(rnorm(10), link = "logit")
#' draw_binary(rnorm(10), link = "probit")
#' draw_binary(runif(10, 0, 1), link = "identity")
#'
draw_binary <- function(vector, link = "logit") {
  if (!link %in% c("logit", "probit", "identity")) {
    stop("Please choose either 'logit', 'probit', or 'identity' as a link.")
  }

  if (mode(vector) != "numeric") {
    stop("Please provide a numeric vector to draw_binary.")
  }

  if (link == "logit") {
    prob <- 1 / (1 + exp(-vector))
  } else if (link == "probit") {
    prob <- pnorm(vector)
  } else if (link == "identity") {
    prob <- vector
    if (!all(0 < prob & prob < 1)) {
      stop(
        "You chose the identity link, which means your vector should be probabilities. Some values of your vector are outside of (0, 1)."
      )
    }
  }

  rbinom(n = length(vector),
         size = 1,
         prob = prob)
}

#' Draw a count variable from a binomial distribution
#'
#' Drawing count variables based on probabilities or latent traits is a common task that can be cumbersome.
#'
#' @param vector vector representing either the latent variable used to draw the count outcome (if link is "logit" or "probit") or the probability for the count outcome (if link is "identity")
#' @param k number of binomial trials, i.e. maximum of the count variable
#' @param link link function between the latent variable and the probability of a postiive outcome, i.e. "logit", "probit", or "identity". For the "identity" link, the latent variable must be a probability.
#'
#' @importFrom stats pnorm
#'
#' @export
#'
#' @examples
#'
#' fabricate_data(N = 10, Y1 = rnorm(N),  Y2 = draw_count(Y1, k = 4))
#' fabricate_data(N = 10, Y1 = rnorm(N),  Y2 = draw_count(Y1, k = 4, link = "probit"))
#' fabricate_data(N = 10, Y1 = runif(N, 0, 1),  Y2 = draw_count(Y1, k = 4, link = "identity"))
#'
#' draw_count(rnorm(10), k = 4)
#' draw_count(rnorm(10), k = 4, link = "logit")
#' draw_count(rnorm(10), k = 4, link = "probit")
#' draw_count(runif(10, 0, 1), k = 4, link = "identity")
draw_count <- function(vector, k, link = "logit") {
  if (!link %in% c("logit", "probit", "identity")) {
    stop("Please choose either 'logit', 'probit', or 'identity' as a link.")
  }

  if (mode(vector) != "numeric") {
    stop("Please provide a numeric vector to draw_binary.")
  }

  if (link == "logit") {
    prob <- 1 / (1 + exp(-vector))
  } else if (link == "probit") {
    prob <- pnorm(vector)
  } else if (link == "identity") {
    prob <- vector
    if (!all(0 < prob & prob < 1)) {
      stop(
        "You chose the identity link, which means your vector should be probabilities. Some values of your vector are outside of (0, 1)."
      )
    }
  }

  rbinom(n = length(vector),
         size = k,
         prob = prob)
}

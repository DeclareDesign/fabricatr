#' Draw a binary variable from a binomial distribution
#'
#' Drawing binary variables based on probabilities or latent traits is a common task that can be cumbersome.
#'
#' @param latent vector representing the latent variable used to draw the count outcome (if link is "logit" or "probit")
#' @param prob vector representing the probability for the count outcome (if link is "identity"). Link is automatically set to "identity" if probabilities is provided.
#' @param link link function between the latent variable and the probability of a postiive outcome, i.e. "logit", "probit", or "identity". For the "identity" link, the latent variable must be a probability.
#'
#' @importFrom stats pnorm
#'
#' @export
#'
#' @examples
#'
#' fabricate(N = 10, Y1 = rnorm(N),  Y2 = draw_binary(Y1))
#' fabricate(N = 10, Y1 = rnorm(N),  Y2 = draw_binary(Y1, link = "logit"))
#' fabricate(N = 10, Y1 = rnorm(N),  Y2 = draw_binary(Y1, link = "probit"))
#' fabricate(N = 10, Y1 = runif(N, 0, 1),  Y2 = draw_binary(prob = Y1))
#'
#' draw_binary(rnorm(10))
#' draw_binary(rnorm(10), link = "logit")
#' draw_binary(rnorm(10), link = "probit")
#' draw_binary(prob = runif(10, 0, 1))
#' draw_binary(prob = runif(10, 0, 1), link = "identity")
#'
draw_binary <- function(latent = NULL, prob = NULL, link = "logit") {
  if (!is.null(prob)) {
    link <- "identity"
  }

  if (all(!is.null(latent),!is.null(prob))) {
    stop("Please either provide a vector of numbers to latent or prob, not both.")
  }

  if (!is.null(latent) & mode(latent) != "numeric") {
    stop("Please provide a numeric vector for the latent variable to draw_binary.")
  }

  if (!is.null(prob) & mode(prob) != "numeric") {
    stop("Please provide a numeric vector to `prob` for draw_binary.")
  }

  if (link == "logit") {
    if (is.null(latent)) {
      stop("Please provide a vector of the latent response when you choose the logit link.")
    }
    prob <- 1 / (1 + exp(-latent))
  } else if (link == "probit") {
    if (is.null(latent)) {
      stop("Please provide a vector of the latent response when you choose the logit link.")
    }
    prob <- pnorm(latent)
  } else if (link == "identity") {
    if (is.null(prob)) {
      stop("Please provide a vector of the probability of a `yes` response when you choose the identity link.")
    }
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
#' @param latent vector representing the latent variable used to draw the count outcome (if link is "logit" or "probit")
#' @param prob vector representing the probability for the count outcome (if link is "identity"). Link is automatically set to "identity" if probabilities is provided.
#' @param k number of binomial trials, i.e. maximum of the count variable
#' @param link link function between the latent variable and the probability of a postiive outcome, i.e. "logit", "probit", or "identity". For the "identity" link, the latent variable must be a probability.
#'
#' @importFrom stats pnorm
#'
#' @export
#'
#' @examples
#'
#' fabricate(N = 10, Y1 = rnorm(N),  Y2 = draw_count(Y1, k = 4))
#' fabricate(N = 10, Y1 = rnorm(N),  Y2 = draw_count(Y1, k = 4, link = "probit"))
#' fabricate(N = 10, Y1 = runif(N, 0, 1),  Y2 = draw_count(prob = Y1, k = 4))
#'
#' draw_count(rnorm(10), k = 4)
#' draw_count(rnorm(10), k = 4, link = "logit")
#' draw_count(rnorm(10), k = 4, link = "probit")
#' draw_count(prob = runif(10, 0, 1), k = 4)
#' draw_count(prob = runif(10, 0, 1), k = 4, link = "identity")
draw_count <- function(latent = NULL, prob = NULL, k, link = "logit") {
  if (!is.null(prob)) {
    link <- "identity"
  }

  if (all(!is.null(latent),!is.null(prob))) {
    stop("Please either provide a vector of numbers to latent or prob, not both.")
  }

  if (!link %in% c("logit", "probit", "identity")) {
    stop("Please choose either 'logit', 'probit', or 'identity' as a link.")
  }

  if (!is.null(latent) & mode(latent) != "numeric") {
    stop("Please provide a numeric vector for the latent variable to draw_count")
  }

  if (!is.null(prob) & mode(prob) != "numeric") {
    stop("Please provide a numeric vector to `prob` for draw_count.")
  }

  if (link == "logit") {
    if (is.null(latent)) {
      stop("Please provide a vector of the latent response when you choose the logit link.")
    }
    prob <- 1 / (1 + exp(-latent))
  } else if (link == "probit") {
    if (is.null(latent)) {
      stop("Please provide a vector of the latent response when you choose the logit link.")
    }
    prob <- pnorm(latent)
  } else if (link == "identity") {
    if (is.null(prob)) {
      stop("Please provide a vector of the probability of a `yes` response when you choose the identity link.")
    }
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

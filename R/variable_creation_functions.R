#' Draw discrete variables including binary, binomial count, poisson count, ordered, and categorical
#'
#' Drawing discrete data based on probabilities or latent traits is a common task that can be cumbersome. \code{draw_binary} is an alias for \code{draw_discrete(type = "binary")} that allows you to draw binary outcomes more easily.
#'
#' @param x vector representing either the latent variable used to draw the count outcome (if link is "logit" or "probit") or the probability for the count outcome (if link is "identity"). For cartegorical distributions x is a matrix with as many columns as possible outcomes.
#' @param type type of discrete outcome to draw, one of 'binary' (or 'bernoulli'), 'binomial', 'categorical', 'ordered' or 'count'
#' @param link link function between the latent variable and the probability of a postiive outcome, i.e. "logit", "probit", or "identity". For the "identity" link, the latent variable must be a probability.
#' @param breaks vector of breaks to cut an ordered latent outcome
#' @param break_labels vector of labels for the breaks for an ordered latent outcome (must be the same length as breaks)
#' @param k the number of trials (zero or more)
#'
#' @importFrom stats pnorm rnorm rpois
#'
#' @export
#'
#' @examples
#' fabricate(N = 3,
#'    p = c(0, .5, 1),
#'    binary = draw_discrete(p))
#'
#' fabricate(N = 3,
#'    p = c(0, .5, 1),
#'    binary = draw_discrete(p, type = "bernoulli"))
#'
#' fabricate(N = 3,
#'    x = 10*rnorm(N),
#'    binary = draw_discrete(x, type = "bernoulli", link = "probit"))
#'
#' fabricate(N = 3,
#'    p = c(0, .5, 1),
#'    binomial = draw_discrete(p, type = "binomial", k = 10))
#'
#' fabricate(N = 3,
#'    x = 5*rnorm(N),
#'    ordered = draw_discrete(x, type = "ordered", breaks = c(-Inf, -1, 1, Inf)))
#'
#' fabricate(N = 3,
#'    x = c(0,5,100),
#'    count = draw_discrete(x, type = "count"))
#'
#' # Categorical
#' fabricate(N = 6, p1 = runif(N), p2 = runif(N), p3 = runif(N),
#'          cat = draw_discrete(cbind(p1, p2, p3), type = "categorical"))
draw_discrete <-
  function(x,
           type = "binary",
           link = "identity",
           breaks = c(-Inf, 0, Inf),
           break_labels = FALSE,
           k = 1) {

    if (!link %in% c("logit", "probit", "identity")) {
      stop("Please choose either 'logit' or 'probit' or 'identity' as a link.")
    }

    if (!type %in% c("binary",
                          "binomial",
                          "bernoulli",
                          "categorical",
                          "ordered",
                          "count")) {
      stop(
        "Please choose either 'binary' (or 'bernoulli'), 'binomial', 'categorical', 'ordered' or 'count' as a data type."
      )
    }

    if (mode(x) != "numeric") {
      stop("Please provide a numeric vector to x.")
    }

    if (link == "logit") {
      prob <- 1 / (1 + exp(-x))
    } else if (link == "probit") {
      prob <- pnorm(x)
    } else if (link == "identity") {
      prob <- x
    }

    if (type %in% c("binary", "bernoulli"))   {
      if (k != 1) {
        k <- 1
        warning("Binary data selected and the number of trials, k, reset to 1.")
      }
      if (link == "identity")
        if (!all(0 <= x & x <= 1)) {
          warning("The identity link requires values between 0 and 1, inclusive")
        }

      n <- length(x)
      out <- rbinom(n, k, prob)

    } else if (type == "binomial")   {
      if (link == "identity")
        if (!all(0 <= x & x <= 1)) {
          warning("The identity link requires values between 0 and 1, inclusive")
        }

      n <- length(x)
      out <- rbinom(n, k, prob)

    } else if (type == "ordered") {
      if (link == "probit"){
        x <- x + rnorm(length(x))
      }

      out <- cut(x, breaks, labels = break_labels) - 1

    } else if (type == "count") {
      if (link != "identity") {
        stop("Count data does not accept link functions.")
      }

      n <- length(x)
      out <- rpois(n, lambda = x)

      ## Categorical

    } else if (type == "categorical") {
      if (is.null(dim(x)))
        stop("For a categorical distribution a matrix of probabilities should be provided")
      if (!all(apply(x, 1, min) > 0)) {
        stop(
          "For a categorical (multinomial) distribution, the elements of x should be positive and sum to a positive number."
        )
      }

      m <- ncol(x)
      rcateg <- function(p)
        sample(1:m, 1, prob = p)
      out <- apply(x, 1, rcateg)
    }

    return(out)
  }

#' @rdname draw_discrete
#' @export
draw_binary <- function(x, link = "identity") {
  return(draw_discrete(
    x,
    type = "binary",
    link = link,
    k = 1
  ))
}

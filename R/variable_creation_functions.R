#' Draw discrete variables including binary, binomial count, poisson count, ordered, and categorical
#'
#' Drawing discrete data based on probabilities or latent traits is a common task that can be cumbersome. \code{draw_binary} is an alias for \code{draw_discrete(type = "binary")} that allows you to draw binary outcomes more easily.
#'
#' @param x vector representing either the latent variable used to draw the count outcome (if link is "logit" or "probit") or the probability for the count outcome (if link is "identity"). For cartegorical distributions x is a matrix with as many columns as possible outcomes.
#' @param N number of units to draw. Defaults to the length of the vector \code{x}
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
           N = length(x),
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
        "Please choose either 'binary' (or 'bernoulli'), 'binomial', 'categorical', 'ordered', or 'count' as a data type."
      )
    }

    if (N %% length(x) & type != "categorical") {
      stop(
        "N is not an even multiple of the length of the vector x."
      )
    }

    if (mode(x) != "numeric") {
      stop("\"x\" must be a number or vector of numbers.")
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
          stop("The identity link requires probability values between 0 and 1, inclusive.")
        }

      out <- rbinom(N, k, prob)

    } else if (type == "binomial")   {
      if (link == "identity")
        if (!all(0 <= x & x <= 1)) {
          stop("The identity link requires values between 0 and 1, inclusive.")
        }

      if(is.vector(k) & length(k)>1) {
        if(N %% length(k)) {
          stop(
            "\"N\" is not an even multiple of the length of the number of trials, \"k\"."
          )
        }
        if(!all(is.numeric(k) & (is.integer(k) | !k%%1))) {
          stop(
            "All numbers of trials should be integer numbers."
          )
        }
      }
      if(!is.null(dim(k))) {
        stop(
          "Number of trials must be an integer or vector, not higher-dimensional."
        )
      }
      if(is.null(k) | is.na(k)) {
        stop(
          "Number of trials must be specified, not null or NA."
        )
      }
      if(is.numeric(k) & !is.integer(k) & k%%1) {
        stop(
          "Number of trials must be an integer."
        )
      }
      if(k <= 0) {
        stop(
          "Number of trials must be a positive integer."
        )
      }

      out <- rbinom(N, k, prob)

    } else if (type == "ordered") {
      if (link == "probit") {
        x <- x + rnorm(N)
      }

      if (is.null(breaks) | any(is.na(breaks))) {
        stop("You must specify numeric breaks for ordered data.")
      }
      if (any(!is.numeric(breaks))) {
        stop("All breaks specified for ordered data must be numeric.")
      }
      if (is.matrix(breaks) | is.data.frame(breaks)) {
        stop("Numeric breaks must be a vector.")
      }
      if (length(breaks) < 3) {
        stop("Numeric breaks for ordered data must be of at least length 3.")
      }
      if (is.unsorted(breaks)) {
        stop("Numeric breaks must be in ascending order.")
      }
      if(any(breaks[1] > x) | any(breaks[length(breaks)] < x)) {
        stop("Numeric break endpoints should be outside min/max of x data range.")
      }
      if(is.vector(break_labels) & !is.logical(break_labels) & all(!is.na(break_labels)) & length(break_labels) != length(breaks)-1) {
        stop("Break labels should be of length one less than breaks.")
      }

      out <- cut(x, breaks, labels = break_labels)

    } else if (type == "count") {
      if (link != "identity") {
        stop("Count data does not accept link functions.")
      }

      if (any(x < 0)) {
        stop(
          "All provided count values must be non-negative."
        )
      }

      out <- rpois(N, lambda = x)

    } else if (type == "categorical") {
      if (link != "identity") {
        stop("Categorical data does not accept link functions.")
      }

      if (is.null(dim(x))) {
        if (is.vector(x) & all(is.numeric(x)) & length(x)>1) {
          x <- matrix(rep(x, N), byrow=TRUE, ncol=length(x), nrow=N)
          warning(
            "For a categorical (multinomial) distribution, a matrix of probabilities should be provided. Data generated by interpreting vector of category probabilities, identical for each observation."
          )
        } else {
          stop(
            "For a categorical (multinomial) distribution, a matrix of probabilities should be provided"
          )
        }
      }
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
draw_binary <- function(x, N = length(x), link = "identity") {
  return(draw_discrete(
    x,
    N = N,
    type = "binary",
    link = link,
    k = 1
  ))
}

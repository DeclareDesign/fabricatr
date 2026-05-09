#' Draw binary (0/1) outcomes
#'
#' @param prob Probability of success. Scalar or per-unit vector. Supports
#'   \code{link} functions: \code{"identity"} (default), \code{"logit"},
#'   \code{"probit"}.
#' @param N Number of draws. Defaults to \code{length(prob)}.
#' @param link Link function name or function. \code{"identity"} expects
#'   \code{prob} to already be a probability. \code{"logit"} and
#'   \code{"probit"} transform a latent variable to the probability scale.
#' @param latent Latent variable (supply instead of \code{prob} when using a
#'   non-identity link).
#' @param quantile_y Optional quantile vector for deterministic draws (used by
#'   \code{correlate}).
#'
#' @return Integer vector of 0s and 1s.
#'
#' @examples
#' fabricate(N = 6, p = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#'           Y = draw_binary(prob = p))
#'
#' # Logit link: supply a latent continuous variable
#' fabricate(N = 100, x = rnorm(N), Y = draw_binary(latent = x, link = "logit"))
#'
#' @importFrom stats rbinom qbinom plogis pnorm
#' @export
draw_binary <- function(prob = apply_link(latent, link),
                        N = length(prob),
                        link = "identity",
                        latent = NULL,
                        quantile_y = NULL) {
  draw_binomial(prob = prob, trials = 1L, N = N, link = link,
                latent = latent, quantile_y = quantile_y)
}

#' Draw binomial counts
#'
#' @param prob Probability of success per trial.
#' @param trials Number of trials per observation (scalar or vector).
#' @param N Number of observations. Defaults to \code{length(prob)}.
#' @param link Link function (\code{"identity"}, \code{"logit"},
#'   \code{"probit"}).
#' @param latent Latent variable (alternative to \code{prob} with non-identity
#'   link).
#' @param quantile_y Optional quantile vector for \code{correlate}.
#'
#' @return Integer vector.
#'
#' @examples
#' fabricate(N = 4, p = c(0.1, 0.3, 0.7, 0.9),
#'           Y = draw_binomial(prob = p, trials = 10))
#'
#' @importFrom stats rbinom qbinom plogis pnorm
#' @export
draw_binomial <- function(prob = apply_link(latent, link),
                          trials = 1L,
                          N = length(prob),
                          link = "identity",
                          latent = NULL,
                          quantile_y = NULL) {
  prob <- resolve_link(prob, link, latent)
  check_prob(prob)
  if (is.null(quantile_y)) {
    rbinom(N, trials, prob)
  } else {
    qbinom(quantile_y, trials, prob)
  }
}

#' Draw Poisson count data
#'
#' @param mean Mean count (lambda). Scalar or per-unit vector.
#' @param N Number of draws. Defaults to \code{length(mean)}.
#' @param link Link function (\code{"identity"}, \code{"log"}).
#' @param latent Latent variable (alternative to \code{mean} with log link).
#' @param quantile_y Optional quantile vector for \code{correlate}.
#'
#' @return Non-negative integer vector.
#'
#' @examples
#' fabricate(N = 5, rate = c(0, 1, 5, 10, 50),
#'           Y = draw_count(mean = rate))
#'
#' @importFrom stats rpois qpois
#' @export
draw_count <- function(mean = apply_link(latent, link),
                       N = length(mean),
                       link = "identity",
                       latent = NULL,
                       quantile_y = NULL) {
  if (!is.null(latent) && identical(link, "log")) mean <- exp(latent)
  if (!is.null(latent) && identical(link, "identity")) mean <- latent
  if (any(mean < 0, na.rm = TRUE)) stop("`mean` must be non-negative for draw_count().")
  if (is.null(quantile_y)) {
    rpois(N, lambda = mean)
  } else {
    qpois(quantile_y, lambda = mean)
  }
}

# Helpers ---------------------------------------------------------------------

apply_link <- function(latent, link) latent  # placeholder; resolved below

resolve_link <- function(prob, link, latent) {
  if (!is.null(latent)) {
    if (identical(link, "logit") || identical(link, "logistic")) {
      return(plogis(latent))
    } else if (identical(link, "probit")) {
      return(pnorm(latent))
    } else {
      return(latent)
    }
  }
  prob
}

check_prob <- function(prob) {
  if (!is.numeric(prob)) stop("`prob` must be numeric.")
  if (any(prob < 0 | prob > 1, na.rm = TRUE)) {
    stop("`prob` values must be between 0 and 1. ",
         "Use link = 'logit' or link = 'probit' to transform a latent variable.")
  }
}

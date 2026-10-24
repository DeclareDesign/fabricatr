#' Draw binary (0/1) outcomes
#'
#' @param prob Probability of success. Scalar or per-unit vector. Already on
#'   the probability scale, so a non-identity \code{link} has nothing to do to
#'   it: supplying both is an error naming \code{latent}.
#' @param N Number of draws. Defaults to \code{length(prob)}.
#' @param link One of \code{"identity"} (default), \code{"logit"},
#'   \code{"logistic"}, \code{"probit"}, or a function, which is applied to
#'   \code{latent} as it stands. Any other name is an error, as it is in
#'   fabricatr 1.x; \code{"logistic"} is a 2.0 synonym for \code{"logit"}
#'   that 1.x does not accept. The link acts on \code{latent} only.
#' @param latent Latent variable on an unbounded scale, mapped to the
#'   probability scale by \code{link}. Supply this rather than \code{prob}
#'   whenever \code{link} is not the identity.
#' @param quantile_y Optional quantile vector for deterministic draws (used by
#'   \code{correlate}).
#'
#' @return An integer vector of 0s and 1s, of length \code{N}, which
#'   defaults to \code{length(prob)}.
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
  check_link_target(link, !missing(prob), latent, "prob", "draw_binary")
  # Named here as well as in draw_binomial(), so that a bad link name reports
  # the function the caller actually wrote.
  check_link_name(link, BINARY_LINKS, "draw_binary")
  draw_binomial(prob = prob, trials = 1L, N = N, link = link,
                latent = latent, quantile_y = quantile_y)
}

#' Draw binomial counts
#'
#' @param prob Probability of success per trial, already on the probability
#'   scale. Supplying it with a non-identity \code{link} is an error naming
#'   \code{latent}.
#' @param trials Number of trials per observation (scalar or vector).
#' @param N Number of observations. Defaults to \code{length(prob)}.
#' @param link One of \code{"identity"} (default), \code{"logit"},
#'   \code{"logistic"}, \code{"probit"}, or a function applied to
#'   \code{latent}. Any other name is an error.
#' @param latent Latent variable on an unbounded scale, mapped to the
#'   probability scale by \code{link}.
#' @param quantile_y Optional quantile vector for \code{correlate}.
#'
#' @return An integer vector of successes out of \code{trials}, of length
#'   \code{N}.
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
  check_link_target(link, !missing(prob), latent, "prob", "draw_binomial")
  prob <- resolve_link(prob, link, latent, BINARY_LINKS, "draw_binomial")
  check_prob(prob)
  if (is.null(quantile_y)) {
    rbinom(N, trials, prob)
  } else {
    qbinom(quantile_y, trials, prob)
  }
}

#' Draw Poisson count data
#'
#' @param mean Mean count (lambda). Scalar or per-unit vector, already the
#'   rate the draw uses. Supplying it with a non-identity \code{link} is an
#'   error naming \code{latent}, because \code{mean = log(3)} with
#'   \code{link = "log"} reads as a request to exponentiate and no range check
#'   can tell it apart from a rate of 1.099.
#' @param N Number of draws. Defaults to \code{length(mean)}.
#' @param link \code{"identity"} (default) or \code{"log"}, or a function
#'   applied to \code{latent}. \code{"log"} exponentiates \code{latent} to
#'   get the rate. Any other name is an error. fabricatr 1.x accepts no link
#'   here at all, answering "Count data does not accept link functions".
#' @param latent Latent variable on the log scale, exponentiated by
#'   \code{link = "log"} to give the rate.
#' @param quantile_y Optional quantile vector for \code{correlate}.
#' @param dispersion Overdispersion, a single non-negative number. The
#'   default 0 is the Poisson, whose variance equals its mean. A positive
#'   value draws from the negative binomial with the same mean and variance
#'   \code{mean + dispersion * mean^2}. It is the reciprocal of the
#'   \code{theta} that \code{MASS::glm.nb()} reports, and the \code{alpha}
#'   of Stata's \code{nbreg}.
#'
#' @return An integer vector of non-negative counts, of length \code{N}.
#'
#' @examples
#' fabricate(N = 5, rate = c(0, 1, 5, 10, 50),
#'           Y = draw_count(mean = rate))
#'
#' # same mean, variance 3 + 0.5 * 3^2 = 7.5 rather than 3
#' y <- draw_count(mean = 3, N = 10000, dispersion = 0.5)
#' c(mean(y), var(y))
#'
#' @importFrom stats rpois qpois rnbinom qnbinom
#' @export
draw_count <- function(mean = apply_link(latent, link),
                       N = length(mean),
                       link = "identity",
                       latent = NULL,
                       quantile_y = NULL,
                       dispersion = 0) {
  check_link_target(link, !missing(mean), latent, "mean", "draw_count")
  mean <- resolve_link(mean, link, latent, COUNT_LINKS, "draw_count")
  if (any(mean < 0, na.rm = TRUE)) stop("`mean` must be non-negative for draw_count().")
  if (!is.numeric(dispersion) || length(dispersion) != 1L ||
      is.na(dispersion) || dispersion < 0 || !is.finite(dispersion)) {
    stop("`dispersion` must be a single non-negative number.", call. = FALSE)
  }
  # The Poisson branch keeps `dispersion = 0` on the same random number stream
  # as before the argument existed.
  if (dispersion == 0) {
    if (is.null(quantile_y)) {
      rpois(N, lambda = mean)
    } else {
      qpois(quantile_y, lambda = mean)
    }
  } else {
    if (is.null(quantile_y)) {
      # rnbinom() returns doubles when given `mu`; rpois() returns integers
      # until a count passes the integer range, and so does this.
      y <- rnbinom(N, size = 1 / dispersion, mu = mean)
      if (all(y <= .Machine$integer.max, na.rm = TRUE)) as.integer(y) else y
    } else {
      qnbinom(quantile_y, size = 1 / dispersion, mu = mean)
    }
  }
}

# Helpers ---------------------------------------------------------------------

# The default of `prob` and `mean`, so that `N` defaults to the latent's length
# when the latent is the only thing supplied. The link itself is applied in
# `resolve_link()`.
apply_link <- function(latent, link) latent

# 1.x validates the link name and 2.0 did not: anything it did not recognise
# fell through to the identity, so `link = "logti"` returned the latent
# untransformed and said nothing. A function is accepted, because 1.x calls it
# and both helpfiles say so.
BINARY_LINKS <- c("identity", "logit", "logistic", "probit")
COUNT_LINKS <- c("identity", "log")

check_link_name <- function(link, allowed, fn) {
  if (is.function(link)) return(invisible(NULL))
  ok <- is.character(link) && length(link) == 1L && !is.na(link) &&
    link %in% allowed
  if (!ok) {
    stop(fn, "(): `link` must be a function or one of ",
         paste0('"', allowed, '"', collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

# A link acts on `latent`, never on the parameter itself, which is already on
# the scale the draw uses. `draw_binary(prob = 0.3, link = "logit")` drew at
# 0.3 where the author meant plogis(0.3) = 0.574, and
# `draw_count(mean = log(3), link = "log")` at 1.099 where they meant 3. The
# range check catches this only where the value cannot be a probability, so it
# never caught a count and caught a binary only for a latent that strayed
# outside [0, 1]. 1.x has the same hole.
check_link_target <- function(link, gave_value, latent, value_arg, fn) {
  if (identical(link, "identity") || !gave_value || !is.null(latent)) {
    return(invisible(NULL))
  }
  stop(fn, "(): `link` acts on `latent`, not on `", value_arg,
       "`, which is already on the scale the draw uses.\n",
       "  Write `", fn, "(latent = ..., link = ...)`.", call. = FALSE)
}

resolve_link <- function(prob, link, latent, allowed, fn) {
  check_link_name(link, allowed, fn)
  if (is.null(latent)) return(prob)
  if (is.function(link)) return(link(latent))
  switch(link,
    identity = latent,
    logit    = plogis(latent),
    logistic = plogis(latent),
    probit   = pnorm(latent),
    log      = exp(latent))
}

check_prob <- function(prob) {
  if (!is.numeric(prob)) stop("`prob` must be numeric.")
  if (any(prob < 0 | prob > 1, na.rm = TRUE)) {
    stop("`prob` values must be between 0 and 1. ",
         "Use link = 'logit' or link = 'probit' to transform a latent variable.")
  }
}

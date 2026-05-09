#' Draw normally distributed data with a target intra-cluster correlation
#'
#' Uses a two-component variance model: each observation's value is the sum of
#' a cluster-level draw (between-cluster variance) and an individual-level draw
#' (within-cluster variance). The ICC equals
#' \eqn{\sigma_b^2 / (\sigma_b^2 + \sigma_w^2)}.
#'
#' Supply exactly two of \code{ICC}, \code{sd} (within-cluster), and
#' \code{sd_between} (between-cluster). The third is implied.
#'
#' @param clusters Vector of cluster IDs (factor, integer, or character).
#' @param ICC Target intra-cluster correlation (0, 1). Required unless both
#'   \code{sd} and \code{sd_between} are provided.
#' @param mean Grand mean or per-cluster mean vector. Default 0.
#' @param sd Within-cluster standard deviation. Default 1 when only ICC is
#'   supplied.
#' @param sd_between Between-cluster standard deviation. Inferred from ICC and
#'   \code{sd} when not provided.
#' @param total_sd If supplied alongside \code{ICC}, rescale the output to have
#'   this overall standard deviation. Cannot be combined with \code{sd} or
#'   \code{sd_between}.
#' @param N Optional; must equal \code{length(clusters)} when provided.
#'
#' @return Numeric vector of the same length as \code{clusters}.
#'
#' @examples
#' clusters <- rep(1:10, each = 20)
#' y <- draw_normal_icc(clusters = clusters, ICC = 0.4)
#' summary(lm(y ~ factor(clusters)))$r.squared  # approx 0.4
#'
#' @importFrom stats rnorm sd
#' @export
draw_normal_icc <- function(clusters,
                            ICC = NULL,
                            mean = 0,
                            sd = NULL,
                            sd_between = NULL,
                            total_sd = NULL,
                            N = NULL) {
  clusters <- tryCatch(
    as.integer(as.factor(clusters)),
    error = function(e) stop("Cannot coerce `clusters` to factor levels.")
  )
  k <- length(unique(clusters))

  if (!is.null(N) && N != length(clusters)) {
    stop("`N` must equal length(clusters).")
  }

  # Resolve sd / sd_between from ICC
  if (!is.null(ICC)) {
    if (ICC <= 0 || ICC >= 1) stop("`ICC` must be strictly between 0 and 1.")
    if (!is.null(total_sd) && (!is.null(sd) || !is.null(sd_between))) {
      stop("When `total_sd` is provided, leave `sd` and `sd_between` blank.")
    }
    if (!is.null(sd) && !is.null(sd_between)) {
      warning("Both `sd` and `sd_between` supplied; ignoring `ICC`.")
    } else if (is.null(sd) && is.null(sd_between)) {
      sd <- 1
    }
    if (is.null(sd)) sd <- sqrt((1 - ICC) * sd_between^2 / ICC)
    if (is.null(sd_between)) sd_between <- sqrt(ICC * sd^2 / (1 - ICC))
  } else {
    if (is.null(sd) || is.null(sd_between)) {
      stop("Provide `ICC`, or provide both `sd` and `sd_between`.")
    }
  }

  # Cluster means
  if (length(mean) == 1) mean <- rep(mean, k)
  ind_mean <- mean[clusters]

  # Two-component draw
  alpha <- rnorm(k, 0, sd_between)[clusters]
  epsilon <- rnorm(length(clusters), 0, sd)
  result <- ind_mean + alpha + epsilon

  if (!is.null(total_sd)) rescale_sd(result, total_sd) else result
}

#' Draw binary data with a target intra-cluster correlation
#'
#' Uses the mixture-of-Bernoullis model: with probability \eqn{\sqrt{ICC}} each
#' unit takes the cluster-level draw; otherwise it takes an independent draw.
#' This produces an expected ICC of the specified value.
#'
#' @param clusters Vector of cluster IDs.
#' @param prob Cluster success probability. Scalar or per-cluster vector.
#'   Default 0.5.
#' @param ICC Target ICC. Default 0 (independent draws).
#' @param N Optional; must equal \code{length(clusters)}.
#'
#' @return Integer vector of 0s and 1s.
#'
#' @examples
#' clusters <- rep(1:10, each = 20)
#' y <- draw_binary_icc(clusters = clusters, prob = 0.5, ICC = 0.3)
#' summary(lm(y ~ factor(clusters)))$r.squared  # approx 0.3
#'
#' @importFrom stats rbinom
#' @export
draw_binary_icc <- function(clusters, prob = 0.5, ICC = 0, N = NULL) {
  uclusters <- sort(unique(clusters))
  cidx <- match(clusters, uclusters)
  n <- length(clusters)
  k <- length(uclusters)

  if (!is.null(N) && N != n) stop("`N` must equal length(clusters).")
  if (length(ICC) != 1 || !is.numeric(ICC) || ICC < 0 || ICC > 1) {
    stop("`ICC` must be a single number in [0, 1].")
  }

  cluster_prob <- if (length(prob) == 1) rep(prob, k) else prob
  ind_prob <- cluster_prob[cidx]

  z_i <- rbinom(k, 1, cluster_prob)[cidx]   # cluster outcome
  y_i <- rbinom(n, 1, ind_prob)             # individual outcome
  u_i <- rbinom(n, 1, sqrt(ICC))            # which to use

  ifelse(u_i, z_i, y_i)
}

# Internal helper
rescale_sd <- function(x, new_sd) {
  m <- mean(x)
  s <- sd(x)
  (x - m) * new_sd / s + m
}

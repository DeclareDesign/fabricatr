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
#'   \code{sd_between}. Note that this is a rescaling of the drawn vector, not
#'   a parameter of the distribution it was drawn from, so the realised
#'   \code{sd()} is exactly \code{total_sd} every time, with none of the
#'   sampling variability a draw of this size would ordinarily show. Across
#'   300 draws of 200 observations the standard deviation of the realised
#'   \code{sd()} is 0, against roughly 0.14 for the same design specified
#'   through \code{sd}. If you are simulating many datasets and want that
#'   variability, set \code{sd} or \code{sd_between} instead: for a target
#'   total \eqn{s} and a given ICC, \eqn{\sigma_{between} = s\sqrt{ICC}} and
#'   \eqn{\sigma_{within} = s\sqrt{1 - ICC}}. The rescaling is affine, so it
#'   leaves the ICC itself untouched.
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
    if (!is.numeric(ICC) || length(ICC) != 1L || is.na(ICC) ||
        ICC < 0 || ICC > 1) {
      stop("`ICC` must be a single number between 0 and 1.", call. = FALSE)
    }
    if (!is.null(total_sd) && (!is.null(sd) || !is.null(sd_between))) {
      stop("When `total_sd` is provided, leave `sd` and `sd_between` blank.")
    }
    if (!is.null(sd) && !is.null(sd_between)) {
      warning("Both `sd` and `sd_between` supplied; ignoring `ICC`.")
    } else if (is.null(sd) && is.null(sd_between)) {
      sd <- 1
    }
    # The endpoints are the degenerate cases, not errors. At ICC = 0 there is
    # no between-cluster variance, so the cluster variable does no work; at
    # ICC = 1 there is no within-cluster variance and every unit in a cluster
    # takes the cluster's value. Solving for the missing standard deviation
    # divides by zero at each end, so name them rather than compute them.
    # `total_sd` still rescales afterwards at the endpoints, exactly as it does
    # at every other ICC.
    if (ICC == 0) {
      if (is.null(sd)) {
        stop("An `ICC` of 0 means no between-cluster variance, so ",
             "`sd_between` cannot also be positive. Supply `sd` instead.",
             call. = FALSE)
      }
      sd_between <- 0
    } else if (ICC == 1) {
      if (is.null(sd_between)) sd_between <- sd
      sd <- 0
    } else {
      if (is.null(sd)) sd <- sqrt((1 - ICC) * sd_between^2 / ICC)
      if (is.null(sd_between)) sd_between <- sqrt(ICC * sd^2 / (1 - ICC))
    }
  } else {
    if (is.null(sd) || is.null(sd_between)) {
      stop("Provide `ICC`, or provide both `sd` and `sd_between`.")
    }
  }

  # Cluster means
  ind_mean <- resolve_cluster_values(mean, clusters, k, "mean")[clusters]

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

  cluster_prob <- resolve_cluster_values(prob, cidx, k, "prob")
  ind_prob <- cluster_prob[cidx]

  z_i <- rbinom(k, 1, cluster_prob)[cidx]   # cluster outcome
  y_i <- rbinom(n, 1, ind_prob)             # individual outcome
  u_i <- rbinom(n, 1, sqrt(ICC))            # which to use

  ifelse(u_i, z_i, y_i)
}

# A cluster-level parameter may arrive already expanded to one value per unit,
# which is what a nested fabricate() hands you: `prob` defined at the cluster
# level is length N by the time the inner level evaluates. Indexing that by
# cluster number would silently read the first k entries and pair the wrong
# probability with each cluster, which is fabricatr#189. Collapse it instead,
# and refuse it if it is not actually constant within cluster.
resolve_cluster_values <- function(values, cidx, k, arg) {
  n <- length(cidx)
  if (length(values) == 1L) return(rep(values, k))
  if (length(values) == k) return(values)
  if (length(values) == n) {
    by_cluster <- split(values, cidx)
    if (any(vapply(by_cluster, function(v) length(unique(v)) > 1L, logical(1)))) {
      stop("`", arg, "` has one value per unit and those values differ within ",
           "a cluster. A cluster-level parameter must be constant inside each ",
           "cluster.", call. = FALSE)
    }
    return(vapply(by_cluster, `[`, numeric(1), 1L, USE.NAMES = FALSE))
  }
  stop("`", arg, "` must have length 1, one value per cluster (", k,
       "), or one value per unit (", n, "). It has ", length(values), ".",
       call. = FALSE)
}

# Internal helper
rescale_sd <- function(x, new_sd) {
  m <- mean(x)
  s <- sd(x)
  (x - m) * new_sd / s + m
}

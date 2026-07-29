#' Draw normally distributed data with a target intra-cluster correlation
#'
#' Uses a two-component variance model: each observation's value is the sum of
#' a cluster-level draw (between-cluster variance) and an individual-level draw
#' (within-cluster variance). The ICC equals
#' \eqn{\sigma_b^2 / (\sigma_b^2 + \sigma_w^2)}.
#'
#' Supply any two of \code{ICC}, \code{sd} (within-cluster),
#' \code{sd_between} (between-cluster), and \code{total_sd}. The other two
#' follow from \eqn{\sigma_{total}^2 = \sigma_b^2 + \sigma_w^2}. Supplying
#' \code{ICC} alone is shorthand for \code{sd = 1}.
#'
#' @param clusters Vector of cluster IDs (factor, integer, or character).
#' @param ICC Target intra-cluster correlation, in \eqn{[0, 1]}. The endpoints
#'   are the degenerate cases rather than errors: at 0 the cluster variable
#'   does no work, and at 1 every unit in a cluster takes the same value.
#' @param mean Grand mean or per-cluster mean vector. Default 0. May be given
#'   per cluster or already expanded to one value per unit.
#' @param sd Within-cluster standard deviation.
#' @param sd_between Between-cluster standard deviation.
#' @param total_sd Overall standard deviation, \eqn{\sqrt{\sigma_b^2 +
#'   \sigma_w^2}}. This is a parameter of the distribution, not a rescaling of
#'   the draw, so the realised \code{sd()} varies from draw to draw as any
#'   other sample statistic does. With \code{ICC}, it gives
#'   \eqn{\sigma_{between} = s\sqrt{ICC}} and
#'   \eqn{\sigma_{within} = s\sqrt{1 - ICC}}. fabricatr instead rescales the
#'   finished vector so that its sample standard deviation is exactly
#'   \code{total_sd} every time; see the package vignette for why this
#'   diverges.
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

  scales <- resolve_icc_scales(ICC, sd, sd_between, total_sd)
  sd         <- scales$within
  sd_between <- scales$between

  # Cluster means
  ind_mean <- resolve_cluster_values(mean, clusters, k, "mean")[clusters]

  # Two-component draw
  alpha <- rnorm(k, 0, sd_between)[clusters]
  epsilon <- rnorm(length(clusters), 0, sd)
  result <- ind_mean + alpha + epsilon

  result
}

# The two-component model has four descriptions of the same pair of variances:
# ICC, within, between, and total, related by total^2 = between^2 + within^2
# and ICC = between^2 / total^2. Any two of them pin the other two, so solve
# rather than draw and then rescale. Rescaling would force the realised sd to
# the target exactly, leaving a simulation with no sampling variability in the
# quantity it is varying.
resolve_icc_scales <- function(ICC, sd, sd_between, total_sd) {
  if (!is.null(ICC) &&
      (!is.numeric(ICC) || length(ICC) != 1L || is.na(ICC) || ICC < 0 || ICC > 1)) {
    stop("`ICC` must be a single number between 0 and 1.", call. = FALSE)
  }
  for (nm in c("sd", "sd_between", "total_sd")) {
    v <- get(nm)
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || is.na(v) || v < 0)) {
      stop("`", nm, "` must be a single non-negative number.", call. = FALSE)
    }
  }

  # Both scale parameters given: the draw is fully determined.
  if (!is.null(sd) && !is.null(sd_between)) {
    if (!is.null(ICC) || !is.null(total_sd)) {
      warning("`sd` and `sd_between` already determine the draw; ignoring ",
              "`ICC` and `total_sd`.", call. = FALSE)
    }
    return(list(within = sd, between = sd_between))
  }

  if (!is.null(ICC)) {
    supplied <- sum(!is.null(sd), !is.null(sd_between), !is.null(total_sd))
    if (supplied > 1L) {
      stop("With `ICC`, supply only one of `sd`, `sd_between`, and ",
           "`total_sd`.", call. = FALSE)
    }
    # ICC alone is shorthand for a unit within-cluster sd, which is what
    # fabricatr does. At ICC = 1 there is no within-cluster variance for that
    # shorthand to describe, so the unit scale goes on the between term.
    if (supplied == 0L) {
      if (ICC == 1) return(list(within = 0, between = 1))
      sd <- 1
    }

    if (!is.null(total_sd)) {
      return(list(within = total_sd * sqrt(1 - ICC),
                  between = total_sd * sqrt(ICC)))
    }
    if (!is.null(sd)) {
      if (ICC == 1) {
        if (sd > 0) {
          stop("An `ICC` of 1 means no within-cluster variance, so `sd` must ",
               "be 0. Supply `total_sd` or `sd_between` to set the scale.",
               call. = FALSE)
        }
        stop("`ICC` of 1 with `sd` of 0 does not pin the scale. Supply ",
             "`total_sd` or `sd_between` as well.", call. = FALSE)
      }
      return(list(within = sd, between = sd * sqrt(ICC / (1 - ICC))))
    }
    if (ICC == 0) {
      if (sd_between > 0) {
        stop("An `ICC` of 0 means no between-cluster variance, so ",
             "`sd_between` must be 0. Supply `total_sd` or `sd` to set the ",
             "scale.", call. = FALSE)
      }
      stop("`ICC` of 0 with `sd_between` of 0 does not pin the scale. Supply ",
           "`total_sd` or `sd` as well.", call. = FALSE)
    }
    return(list(within = sd_between * sqrt((1 - ICC) / ICC),
                between = sd_between))
  }

  if (!is.null(total_sd) && !is.null(sd)) {
    if (sd > total_sd) {
      stop("`sd` cannot exceed `total_sd`.", call. = FALSE)
    }
    return(list(within = sd, between = sqrt(total_sd^2 - sd^2)))
  }
  if (!is.null(total_sd) && !is.null(sd_between)) {
    if (sd_between > total_sd) {
      stop("`sd_between` cannot exceed `total_sd`.", call. = FALSE)
    }
    return(list(within = sqrt(total_sd^2 - sd_between^2), between = sd_between))
  }

  stop("Supply any two of `ICC`, `sd`, `sd_between`, and `total_sd`.",
       call. = FALSE)
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


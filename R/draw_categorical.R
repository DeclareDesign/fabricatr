#' Draw categorical outcomes
#'
#' Draws from a multinomial distribution. Supply a probability matrix (one row
#' per observation, one column per category) or a single probability vector
#' shared across all observations.
#'
#' @param prob Probability specification. Either:
#'   \itemize{
#'     \item A numeric vector of category probabilities (same for all
#'       observations; \code{N} must be supplied), or
#'     \item A matrix with \code{N} rows and one column per category.
#'   }
#'   Rows need not sum to 1 — they are normalised internally.
#' @param N Number of observations. Required when \code{prob} is a vector.
#' @param labels Optional character vector of category labels (length must
#'   equal the number of categories). When supplied, returns an ordered
#'   factor.
#'
#' @return Integer vector (1, 2, ...) or ordered factor if \code{labels} is
#'   supplied.
#'
#' @examples
#' # Shared probabilities across all units
#' fabricate(N = 100, cat = draw_categorical(prob = c(0.2, 0.5, 0.3), N = N))
#'
#' # Per-unit probability matrix
#' fabricate(N = 4, p1 = runif(N), p2 = runif(N), p3 = runif(N),
#'           cat = draw_categorical(prob = cbind(p1, p2, p3)))
#'
#' # With labels -> ordered factor
#' fabricate(N = 100,
#'           edu = draw_categorical(prob = c(0.3, 0.5, 0.2), N = N,
#'                                  labels = c("low", "medium", "high")))
#'
#' @export
draw_categorical <- function(prob, N = NULL, labels = NULL) {
  # Coerce vector to matrix (same probs for all units)
  if (is.null(dim(prob))) {
    if (!is.numeric(prob) || length(prob) < 2) {
      stop("`prob` must be a numeric vector of length >= 2 or a matrix.")
    }
    if (is.null(N)) stop("Supply `N` when `prob` is a vector of category probabilities.")
    prob <- matrix(rep(prob, N), nrow = N, byrow = TRUE)
  }

  if (!is.numeric(prob) || any(prob < 0, na.rm = TRUE)) {
    stop("`prob` must be a non-negative numeric matrix.")
  }
  if (is.null(N)) N <- nrow(prob)
  if (nrow(prob) != N) stop("nrow(prob) must equal N.")

  k <- ncol(prob)
  if (!is.null(labels) && length(labels) != k) {
    stop("length(labels) must equal the number of categories (", k, ").")
  }

  # Normalise rows (allows unnormalised input)
  row_sums <- rowSums(prob)
  if (any(row_sums == 0)) stop("At least one row of `prob` sums to zero.")
  prob <- prob / row_sums

  draws <- apply(prob, 1, function(p) sample.int(k, 1L, prob = p))

  if (!is.null(labels)) {
    factor(draws, levels = seq_len(k), labels = labels, ordered = TRUE)
  } else {
    draws
  }
}

#' Draw ordered categorical outcomes from a latent variable
#'
#' Cuts a continuous latent variable at the supplied \code{breaks} to produce
#' ordered discrete categories. A natural companion to latent-variable
#' specifications of survey responses.
#'
#' @param x Latent continuous variable (numeric vector).
#' @param breaks Numeric vector of cut-points in ascending order. The vector
#'   should span the range of \code{x}; values outside the range are placed in
#'   the outermost categories unless \code{strict = TRUE}.
#' @param labels Optional character vector of category labels. Length must
#'   equal \code{length(breaks) + 1}. When supplied, returns an ordered factor.
#' @param N Length of \code{x}. Inferred automatically; rarely needs to be
#'   set explicitly.
#' @param strict If \code{TRUE}, observations outside \code{breaks} are coded
#'   \code{NA} instead of being placed in the outermost category.
#' @param latent Alias for \code{x} (kept for compatibility).
#' @param link Ignored (identity only); present for API consistency.
#'
#' @return Integer vector or ordered factor.
#'
#' @examples
#' fabricate(N = 200, x = rnorm(N),
#'           resp = draw_ordered(x, breaks = c(-1, 0, 1),
#'                               labels = c("disagree", "neutral",
#'                                          "agree", "strongly agree")))
#'
#' @export
draw_ordered <- function(x = latent,
                         breaks,
                         labels = NULL,
                         N = length(x),
                         strict = FALSE,
                         latent = NULL,
                         link = "identity") {
  if (missing(breaks) || is.null(breaks) || any(is.na(breaks))) {
    stop("Supply numeric `breaks` to draw_ordered().")
  }
  if (!is.numeric(breaks)) stop("`breaks` must be numeric.")
  if (is.unsorted(breaks)) stop("`breaks` must be in ascending order.")

  n_cats <- length(breaks) + 1L
  if (!is.null(labels) && length(labels) != n_cats) {
    stop("length(labels) must equal length(breaks) + 1 = ", n_cats, ".")
  }

  vals <- findInterval(x, breaks) + 1L

  if (strict) {
    vals[x < breaks[1] | x > breaks[length(breaks)]] <- NA_integer_
  }

  if (!is.null(labels)) {
    factor(vals, levels = seq_len(n_cats), labels = labels, ordered = TRUE)
  } else {
    vals
  }
}

#' Recode a latent variable into a Likert response
#'
#' A convenience wrapper around \code{draw_ordered} that constructs equally
#' spaced breaks between \code{min} and \code{max}.
#'
#' @param x Latent numeric variable.
#' @param min,max Range of \code{x}.
#' @param bins Number of Likert categories.
#' @param breaks Manual break vector (alternative to \code{min}/\code{max}/
#'   \code{bins}).
#' @param labels Optional category labels.
#'
#' @return Integer vector or ordered factor.
#'
#' @examples
#' fabricate(N = 100, x = rnorm(N),
#'           likert = draw_likert(x, min = -3, max = 3, bins = 5))
#'
#' @export
draw_likert <- function(x, min = NULL, max = NULL, bins = NULL,
                        breaks = NULL, labels = NULL) {
  if (is.null(breaks)) {
    if (is.null(min) || is.null(max) || is.null(bins)) {
      stop("Provide either `breaks` or all of `min`, `max`, and `bins`.")
    }
    breaks <- seq(min, max, length.out = bins + 1L)
    # Drop the endpoints so all values are binned
    breaks <- breaks[-c(1L, length(breaks))]
  }
  draw_ordered(x, breaks = breaks, labels = labels)
}

#' Split data into quantile buckets
#'
#' @param x Numeric vector.
#' @param type Number of buckets (2 = median split, 3 = terciles, etc.).
#'
#' @return Ordered factor with levels 1 through \code{type}.
#'
#' @examples
#' split_quantile(rnorm(100), type = 4)
#'
#' @importFrom stats quantile
#' @export
split_quantile <- function(x, type) {
  if (length(x) < 2) stop("`x` must have length >= 2.")
  if (!is.numeric(type) || length(type) != 1 || type < 2) {
    stop("`type` must be a single integer >= 2.")
  }
  probs <- seq(0, 1, length.out = type + 1L)
  cut(x, breaks = quantile(x, probs = probs),
      labels = seq_len(type), include.lowest = TRUE, ordered_result = TRUE)
}

#' Draw quantile bucket assignments
#'
#' Draws N observations and assigns them to \code{type} equally sized
#' quantile buckets via a uniform latent variable.
#'
#' @param type Number of buckets.
#' @param N Number of observations.
#'
#' @return Ordered factor.
#'
#' @examples
#' draw_quantile(type = 4, N = 100)
#'
#' @importFrom stats runif
#' @export
draw_quantile <- function(type, N) {
  if (!is.numeric(N) || length(N) != 1 || N < 1) {
    stop("`N` must be a single positive integer.")
  }
  if (!is.numeric(type) || length(type) != 1 || type < 2 || type >= N) {
    stop("`type` must be a single integer between 2 and N-1.")
  }
  split_quantile(runif(N), type = type)
}

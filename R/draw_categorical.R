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
#'   Rows need not sum to 1; they are normalised internally.
#' @param N Number of observations. Required when \code{prob} is a vector.
#' @param labels Optional character vector of category labels (length must
#'   equal the number of categories). When supplied, returns a factor. The
#'   categories are nominal, so the factor is unordered, as it is in
#'   fabricatr 1.x.
#' @param category_labels fabricatr 1.x's name for \code{labels}. Accepted
#'   with a warning.
#'
#' @return An integer vector of category indices \code{1}, \code{2}, and so
#'   on, one per row of \code{prob}, or an unordered factor with those
#'   \code{labels} when they are supplied.
#'
#' @examples
#' # Shared probabilities across all units
#' fabricate(N = 100, cat = draw_categorical(prob = c(0.2, 0.5, 0.3), N = N))
#'
#' # Per-unit probability matrix
#' fabricate(N = 4, p1 = runif(N), p2 = runif(N), p3 = runif(N),
#'           cat = draw_categorical(prob = cbind(p1, p2, p3)))
#'
#' # With labels -> factor
#' fabricate(N = 100,
#'           edu = draw_categorical(prob = c(0.3, 0.5, 0.2), N = N,
#'                                  labels = c("low", "medium", "high")))
#'
#' @export
draw_categorical <- function(prob, N = NULL, labels = NULL,
                             category_labels = NULL) {
  labels <- absorb_legacy_labels(labels, category_labels, "category_labels",
                                 sys.call())
  if (!is.null(N) && (!is.numeric(N) || length(N) != 1L || is.na(N) || N < 1)) {
    stop("`N` must be a single positive integer.")
  }
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
  if (anyNA(prob)) stop("`prob` must not contain NA.")
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
    factor(draws, levels = seq_len(k), labels = labels)
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
#'   the outermost categories unless \code{strict = TRUE}. An infinite
#'   endpoint bounds the scale rather than cutting it, so
#'   \code{c(-Inf, 0, Inf)} gives the same two categories as \code{0}.
#' @param labels Optional character vector of category labels, one per
#'   category. That is \code{length(breaks) + 1} for interior cut-points, one
#'   fewer for each infinite endpoint, and \code{length(breaks) - 1} when
#'   \code{strict = TRUE}. When supplied, returns an ordered factor.
#' @param N Must equal \code{length(x)} if it is given at all, and is an
#'   error otherwise. It cannot change how many values come back, which is
#'   always \code{length(x)}: unlike \code{draw_binary()} and the rest of the
#'   family, \code{draw_ordered()} has never recycled to \code{N}, in either
#'   version. 1.x checks \code{N} against a recycling rule it does not apply.
#' @param strict If \code{TRUE}, observations outside \code{breaks} are coded
#'   \code{NA} instead of being placed in the outermost category.
#' @param latent Alias for \code{x} (kept for compatibility).
#' @param link Must be \code{"identity"}, the only link this function has:
#'   \code{breaks} cut the latent variable on the scale it arrives. Anything
#'   else is an error, as it is in fabricatr 1.x.
#' @param break_labels fabricatr 1.x's name for \code{labels}. Accepted with
#'   a warning.
#'
#' @return An integer vector of category indices, one per element of
#'   \code{x}, or an ordered factor with those \code{labels} when they are
#'   supplied. With \code{strict = TRUE}, values outside \code{breaks} are
#'   \code{NA}.
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
                         link = "identity",
                         break_labels = NULL) {
  labels <- absorb_legacy_labels(labels, break_labels, "break_labels",
                                 sys.call())
  # 1.x refuses any other link here, and refusing is right: `breaks` are
  # cut-points on the scale `x` arrives on, so a link has nothing to act on.
  # Accepting one silently, which is what this did, returns the identity
  # answer to someone who asked for a transformation.
  if (!identical(link, "identity")) {
    stop("draw_ordered() only allows the \"identity\" link: `breaks` cut the ",
         "latent variable on the scale it arrives.", call. = FALSE)
  }
  # `N` has never changed the length here, in either version, so it may only
  # confirm it. 1.x validates it against a recycling rule it never applies,
  # which is how it came to look like an argument that works.
  if (!missing(N) && !identical(as.integer(N), length(x))) {
    stop("draw_ordered(): `N` must equal length(x) (", length(x),
         "). It does not recycle `x`.", call. = FALSE)
  }
  if (missing(breaks) || is.null(breaks) || any(is.na(breaks))) {
    stop("Supply numeric `breaks` to draw_ordered().")
  }
  if (!is.numeric(breaks)) stop("`breaks` must be numeric.")
  if (is.unsorted(breaks)) stop("`breaks` must be in ascending order.")

  # `breaks` may be interior cut-points, or may carry infinite endpoints that
  # bound the scale rather than cut it. Each infinite endpoint removes a
  # category, and `strict = TRUE` removes both, by sending everything outside
  # `breaks` to NA. findInterval() returns 0 only for a value below breaks[1],
  # so the offset that makes the codes 1-based is 1 exactly when that region
  # is still a category of its own. 1.x reads the lower end correctly but
  # applies the same test to a trailing Inf, which returns 0-based codes for
  # breaks = c(-1, 0, Inf); the lower end alone decides the offset.
  open_below <- strict || (is.infinite(breaks[1L]) && breaks[1L] < 0)
  open_above <- strict || (is.infinite(breaks[length(breaks)]) &&
                             breaks[length(breaks)] > 0)
  n_cats <- length(breaks) + 1L - open_below - open_above
  if (!is.null(labels) && length(labels) != n_cats) {
    stop("length(labels) must equal the number of categories, ", n_cats, ".")
  }

  vals <- findInterval(x, breaks) + if (open_below) 0L else 1L

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
#' @return An integer vector of category indices, one per element of
#'   \code{x}, or an ordered factor with those \code{labels} when they are
#'   supplied.
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
    for (nm in c("min", "max", "bins")) {
      v <- get(nm)
      if (!is.numeric(v) || length(v) != 1L || !is.finite(v)) {
        stop("`", nm, "` must be a single finite number.", call. = FALSE)
      }
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
#' @return A factor with levels \code{1} through \code{type}, one per element
#'   of \code{x}. It is deliberately \emph{unordered}, as in fabricatr 1.x:
#'   an ordered factor would make \code{lm} fit polynomial contrasts
#'   (\code{q.L}, \code{q.Q}, \code{q.C}) where the same script under 1.x
#'   gets treatment contrasts (\code{q2}, \code{q3}, \code{q4}), and report
#'   different coefficients without saying why.
#'
#' @examples
#' split_quantile(rnorm(100), type = 4)
#'
#' @importFrom stats quantile
#' @export
split_quantile <- function(x, type) {
  if (length(x) < 2) stop("`x` must have length >= 2.")
  if (!is.numeric(type) || length(type) != 1 || is.na(type) ||
      type != round(type) || type < 2) {
    stop("`type` must be a single integer >= 2.")
  }
  probs <- seq(0, 1, length.out = type + 1L)
  # Unordered, as in fabricatr. An ordered factor would change the contrasts
  # R picks for it: lm(Y ~ q) would fit polynomial terms (q.L, q.Q, q.C)
  # rather than the treatment contrasts (q2, q3, q4) the same script gets
  # under fabricatr, and report different coefficients without saying why.
  cut(x, breaks = quantile(x, probs = probs),
      labels = seq_len(type), include.lowest = TRUE)
}

#' Draw quantile bucket assignments
#'
#' Draws N observations and assigns them to \code{type} equally sized
#' quantile buckets via a uniform latent variable.
#'
#' @param type Number of buckets.
#' @param N Number of observations.
#'
#' @return A factor of length \code{N} with levels \code{1} through
#'   \code{type}, unordered for the reason given under
#'   \code{\link{split_quantile}}.
#'
#' @examples
#' draw_quantile(type = 4, N = 100)
#'
#' @importFrom stats runif
#' @export
draw_quantile <- function(type, N) {
  if (!is.numeric(N) || length(N) != 1 || is.na(N) || N < 1) {
    stop("`N` must be a single positive integer.")
  }
  if (!is.numeric(type) || length(type) != 1 || is.na(type) ||
      type != round(type) || type < 2 || type >= N) {
    stop("`type` must be a single integer between 2 and N-1.")
  }
  split_quantile(runif(N), type = type)
}

#' Draw multivariate random variables
#'
#' A formula interface for functions that return a matrix of draws (e.g.
#' \code{MASS::mvrnorm}, \code{extraDistr::rmnom}). The left-hand side of the
#' formula names the resulting columns; the right-hand side is any expression
#' that returns an N-by-k matrix.
#'
#' When called inside \code{fabricate()} without a name on the left of \code{=},
#' the resulting columns are appended directly to the data frame.
#'
#' @param formula A two-sided formula. LHS: either a single bare name (used as
#'   a column prefix, producing \code{name_1}, \code{name_2}, ...) or a
#'   \code{c(name1, name2, ...)} call giving explicit column names. RHS: any
#'   expression returning an N-by-k numeric matrix.
#' @param sep Separator between prefix and index when LHS is a single name.
#'   Default \code{"_"}.
#'
#' @return A tibble with one column per name on the left-hand side of the
#'   formula, in that order, and one row per row of the matrix the right-hand
#'   side returns.
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   S <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
#'   draw_multivariate(c(Y1, Y2) ~ MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = S))
#' }
#'
#' # Inside fabricate (unnamed -> columns appended automatically)
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   S <- matrix(c(1, 0.4, 0.4, 1), 2, 2)
#'   fabricate(N = 50,
#'             draw_multivariate(c(X, Y) ~ MASS::mvrnorm(n = N, mu = c(0, 0), Sigma = S)))
#' }
#'
#' @importFrom rlang f_lhs f_rhs eval_tidy call_args as_label
#' @export
draw_multivariate <- function(formula, sep = "_") {
  mat <- rlang::eval_tidy(rlang::f_rhs(formula), env = environment(formula))
  if (!is.matrix(mat) && !is.data.frame(mat)) {
    stop("The RHS of the formula must return a matrix or data frame.")
  }
  mat <- as.matrix(mat)

  lhs <- rlang::f_lhs(formula)
  if (is.null(lhs)) {
    stop("Provide column names on the LHS of the formula: ",
         "either a prefix name or c(name1, name2, ...).")
  }

  if (inherits(lhs, "name")) {
    nms <- paste0(as.character(lhs), sep, seq_len(ncol(mat)))
  } else if (inherits(lhs, "call")) {
    nms <- vapply(rlang::call_args(lhs), rlang::as_label, character(1))
    if (length(nms) != ncol(mat)) {
      stop("LHS names (", length(nms), ") do not match matrix columns (",
           ncol(mat), ").")
    }
  } else {
    stop("Unrecognised LHS in draw_multivariate formula.")
  }

  colnames(mat) <- nms
  tibble::as_tibble(mat)
}

#' Generate a variable correlated with an existing variable
#'
#' Uses a Gaussian copula to produce a draw from \code{draw_handler} whose
#' rank correlation with \code{given} is approximately \code{rho}. Works with
#' any \code{draw_*} function that accepts a \code{quantile_y} argument, and
#' with base R random-number generators (e.g. \code{rnorm}, \code{rpois}).
#'
#' @param draw_handler Unquoted function name. Either one of the three
#'   \code{draw_*} functions that take a \code{quantile_y} argument
#'   (\code{draw_binary}, \code{draw_binomial}, \code{draw_count}) or a base R
#'   \code{r*} generator with a \code{q*} counterpart (\code{rnorm},
#'   \code{rpois}, \code{rbinom}, and the rest of the \pkg{stats} family).
#'   \code{draw_ordered}, \code{draw_likert}, \code{draw_categorical},
#'   \code{draw_quantile}, and the two ICC draws take no quantile argument and
#'   are refused with a message saying so.
#' @param ... Arguments forwarded to \code{draw_handler} (e.g. \code{prob},
#'   \code{mean}).
#' @param given Reference vector; the new variable will be rank-correlated with
#'   this.
#' @param rho Correlation of the Gaussian copula, in \eqn{[-1, 1]}. It is not
#'   the realized Spearman correlation, which is smaller by the copula's own
#'   relation, \eqn{(6/\pi)\arcsin(\rho/2)}: \code{rho = 0.7} lands at 0.68
#'   and \code{rho = 0.3} at 0.288. The realized Pearson correlation is
#'   \code{rho}. fabricatr 1.x behaves identically.
#'
#' @return A double vector of length \code{length(given)}. Note that the type
#'   is double whatever \code{draw_handler} is: \code{draw_binary},
#'   \code{draw_binomial}, and \code{draw_count} each return an integer vector
#'   when called directly and a double one when routed through here, because
#'   the copula reaches them through a quantile function. The values are
#'   unaffected. An \code{NA} in \code{given} yields an \code{NA} in the
#'   result at that position.
#'
#' @examples
#' score  <- rnorm(100, mean = 75, sd = 10)
#' offers <- correlate(draw_count, mean = 3, given = score, rho = 0.6)
#' cor(score, offers, method = "spearman")
#'
#' # Works with base R generators too
#' y2 <- correlate(rnorm, mean = 0, sd = 1, given = score, rho = -0.5)
#'
#' @importFrom stats qnorm pnorm rnorm
#' @importFrom rlang is_closure
#' @export
correlate <- function(draw_handler, ..., given, rho) {
  if (!rlang::is_closure(draw_handler)) {
    stop("`draw_handler` must be a function (unquoted).")
  }
  if (!is.numeric(rho) || length(rho) != 1 || is.na(rho) ||
      rho < -1 || rho > 1) {
    stop("`rho` must be a single number in [-1, 1].")
  }
  if (is.null(given) || !is.null(dim(given))) {
    stop("`given` must be a non-null vector.")
  }

  n <- length(given)
  # `rank()` defaults to na.last = TRUE, which gives an NA in `given` the top
  # rank and draws a correspondingly extreme value for it, so the missingness
  # disappears into a number that looks real. Rank the observed values only and
  # carry the NA through to the result. With no NAs this is the same arithmetic
  # on the same random stream as before.
  obs  <- !is.na(given)
  sn_x <- rep(NA_real_, n)
  # Map given to standard normal via rank-based ECDF (avoids infinite z-scores)
  sn_x[obs] <- qnorm(rank(given[obs]) / (sum(obs) + 1))
  # Conditional distribution of Y | X for bivariate standard normal
  sn_y <- rep(NA_real_, n)
  sn_y[obs] <- rnorm(sum(obs), rho * sn_x[obs], sqrt(1 - rho^2))
  q_y  <- pnorm(sn_y)

  # If draw_handler accepts quantile_y (our draw_* functions), use it directly
  if ("quantile_y" %in% names(formals(draw_handler))) {
    return(draw_handler(..., quantile_y = q_y))
  }

  # Otherwise try to map r* -> q* for base R generators
  q_fn <- lookup_quantile_function(draw_handler)
  if (is.function(q_fn)) {
    return(q_fn(p = q_y, ...))
  }

  stop("`draw_handler` must be a draw_*() function or a base R r*() function ",
       "(e.g. rnorm, rpois). Custom functions need a `quantile_y` argument.")
}

# Map r* functions to their q* counterparts ----------------------------------
lookup_quantile_function <- local({
  r_fns <- list(
    stats::rbeta, stats::rbinom, stats::rcauchy, stats::rchisq,
    stats::rexp,  stats::rf,     stats::rgamma,  stats::rgeom,
    stats::rhyper, stats::rlnorm, stats::rnbinom, stats::rnorm,
    stats::rpois, stats::rt,     stats::runif,   stats::rweibull
  )
  q_fns <- list(
    stats::qbeta, stats::qbinom, stats::qcauchy, stats::qchisq,
    stats::qexp,  stats::qf,     stats::qgamma,  stats::qgeom,
    stats::qhyper, stats::qlnorm, stats::qnbinom, stats::qnorm,
    stats::qpois, stats::qt,     stats::qunif,   stats::qweibull
  )
  function(f) {
    idx <- which(vapply(r_fns, identical, FALSE, f))
    if (length(idx) == 0) NULL else q_fns[[idx[1]]]
  }
})

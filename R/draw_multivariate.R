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
#' @return A tibble with one column per variable drawn.
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
#' @param draw_handler Unquoted function name: a \code{draw_*} function or a
#'   base R \code{r*} function.
#' @param ... Arguments forwarded to \code{draw_handler} (e.g. \code{prob},
#'   \code{mean}).
#' @param given Reference vector; the new variable will be rank-correlated with
#'   this.
#' @param rho Target Spearman rank correlation in \eqn{[-1, 1]}.
#'
#' @return Numeric vector of length \code{length(given)}.
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
  if (!is.numeric(rho) || length(rho) != 1 || rho < -1 || rho > 1) {
    stop("`rho` must be a single number in [-1, 1].")
  }
  if (is.null(given) || !is.null(dim(given))) {
    stop("`given` must be a non-null vector.")
  }

  n <- length(given)
  # Map given to standard normal via rank-based ECDF (avoids infinite z-scores)
  sn_x <- qnorm(rank(given) / (n + 1))
  # Conditional distribution of Y | X for bivariate standard normal
  sn_y <- rnorm(n, rho * sn_x, sqrt(1 - rho^2))
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

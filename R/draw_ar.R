#' Draw normally distributed data with serial correlation within clusters
#'
#' Draws a first-order autoregressive (AR(1)) process separately inside each
#' cluster, the error structure of a panel in which a unit's shocks persist
#' from one period to the next. Within a cluster, rows are ordered by
#' \code{time}; the first value is a draw from the stationary distribution and
#' each later one is
#' \deqn{e_t = r e_{t-1} + \sqrt{1 - r^2} \, \sigma u_t, \quad r = \rho^{\Delta t},}
#' where \eqn{\Delta t} is the gap in \code{time} since the unit's previous
#' observation and \eqn{u_t} is standard normal. Because the process starts at
#' its stationary distribution, every period has standard deviation
#' \code{sd} and there is no burn-in. Two observations of the same unit
#' \eqn{k} periods apart have correlation \eqn{\rho^k}. With equally spaced
#' periods this is the ordinary AR(1), and with gaps it is the continuous-time
#' version, so an unbalanced panel needs no special handling. This is the
#' process that \code{nlme::corAR1()} and \code{nlme::corCAR1()} assume.
#'
#' The result is a latent normal variable. For a binary or count outcome with
#' the same persistence, pass it on: as \code{latent} to
#' \code{\link{draw_binary}} with a probit link, or as
#' \code{quantile_y = pnorm(e)} to \code{\link{draw_count}} and the other
#' \code{draw_*} functions. The serial correlation of the outcome is then
#' weaker than \code{rho}, as with \code{\link{correlate}}.
#'
#' The function draws persistent errors, not state dependence: a model in
#' which \eqn{Y_t} depends on \eqn{Y_{t-1}} itself is a different model.
#'
#' @param clusters Vector of unit IDs. Each unit gets its own independent
#'   series.
#' @param time Vector giving each row's position in its unit's series. Numeric,
#'   a \code{Date} (read in days, so \code{rho} is the correlation from one
#'   day to the next), or character or factor values that read as numbers,
#'   which is what \code{\link{cross_levels}} produces for a level's ID. A
#'   factor is read by its labels, never by the order of its levels. A unit may
#'   not have two rows at the same time.
#' @param rho Correlation between consecutive periods, strictly between -1 and
#'   1. A negative \code{rho} needs whole-number gaps in \code{time}, since
#'   \eqn{\rho^{\Delta t}} is undefined otherwise.
#' @param mean Mean, a scalar or one value per row. Default 0.
#' @param sd Standard deviation of the process in every period. Default 1.
#'   This is the marginal standard deviation, the spread of the values in any
#'   one period, so changing \code{rho} changes persistence and leaves the
#'   variance alone. \code{stats::arima.sim()} takes the standard deviation
#'   of each period's new shock instead, which is \code{sd * sqrt(1 - rho^2)}
#'   here: the same number there gives a process \code{1 / sqrt(1 - rho^2)}
#'   times wider, 2.3 times at \code{rho = 0.9}.
#'
#' @return A double vector of the same length as \code{clusters}, in the
#'   order the rows were given.
#'
#' @examples
#' panel <- fabricate(
#'   units = declare_level(N = 50, u = rnorm(N)),
#'   periods = declare_level(N = 10),
#'   obs = cross_levels(
#'     .by = c("units", "periods"),
#'     e = draw_normal_ar(clusters = units, time = periods, rho = 0.7),
#'     Y = u + e
#'   )
#' )
#'
#' # the lag-1 correlation of e within unit is close to 0.7
#' panel <- panel[order(panel$units, as.numeric(panel$periods)), ]
#' same_unit <- panel$units[-1] == panel$units[-nrow(panel)]
#' cor(panel$e[-1][same_unit], panel$e[-nrow(panel)][same_unit])
#'
#' @importFrom stats rnorm
#' @export
draw_normal_ar <- function(clusters, time, rho, mean = 0, sd = 1) {
  n <- length(clusters)

  if (!is.numeric(rho) || length(rho) != 1L || is.na(rho) || abs(rho) >= 1) {
    stop("`rho` must be a single number strictly between -1 and 1.",
         call. = FALSE)
  }
  if (!is.numeric(sd) || length(sd) != 1L || is.na(sd) || sd < 0) {
    stop("`sd` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(mean) || !(length(mean) %in% c(1L, n))) {
    stop("`mean` must be numeric, with length 1 or one value per row (", n,
         ").", call. = FALSE)
  }
  if (length(time) != n) {
    stop("`time` must have one value per row (", n, "). It has ",
         length(time), ".", call. = FALSE)
  }
  if (anyNA(clusters)) stop("`clusters` has missing values.", call. = FALSE)

  if (inherits(time, "Date")) {
    time <- as.numeric(time)
  } else if (is.factor(time) || is.character(time)) {
    time_num <- suppressWarnings(as.numeric(as.character(time)))
    if (anyNA(time_num[!is.na(time)])) {
      stop("`time` must be numeric, or character or factor values that read ",
           "as numbers.", call. = FALSE)
    }
    time <- time_num
  }
  if (!is.numeric(time)) {
    stop("`time` must be numeric, a Date, or character or factor values ",
         "that read as numbers.", call. = FALSE)
  }
  if (anyNA(time)) stop("`time` has missing values.", call. = FALSE)

  if (n == 0L) return(numeric(0))

  cidx <- as.integer(as.factor(clusters))
  ord <- order(cidx, time)
  cs <- cidx[ord]
  first <- c(TRUE, cs[-1] != cs[-n])
  gap <- c(NA, diff(time[ord]))
  gap[first] <- NA

  if (any(gap == 0, na.rm = TRUE)) {
    stop("A unit has two rows at the same `time`. Each unit's times must be ",
         "distinct.", call. = FALSE)
  }
  if (rho < 0 && any(gap != round(gap), na.rm = TRUE)) {
    stop("A negative `rho` needs whole-number gaps in `time`, since rho ",
         "raised to a fractional power is undefined.", call. = FALSE)
  }

  # The correlation to the previous observation decays with the gap, so an
  # unbalanced panel is the same process observed at fewer points.
  r <- rho^gap
  z <- rnorm(n)
  e <- numeric(n)
  for (i in seq_len(n)) {
    e[i] <- if (first[i]) z[i] else r[i] * e[i - 1L] + sqrt(1 - r[i]^2) * z[i]
  }

  out <- numeric(n)
  out[ord] <- e
  mean + sd * out
}

#' Perform generation of a correlated random variable.
#'
#' In order to generate a random variable of a specific distribution based on
#' another variable of any distribution and a correlation coefficient `rho`,
#' we map the first, known variable into the standard normal space via affine
#' transformation, generate the conditional distribution of the resulting
#' variable as a standard normal, and then map that standard normal back to
#' the target distribution. The result ensures, in expectation, a rank-order
#' correlation of `rho`.
#'
#' @param draw_handler The unquoted name of a function to generate data.
#' Currently, `draw_binary`, `draw_binomial`, and `draw_count` are supported.
#' @param ... The arguments to draw_handler (e.g. `prob`, `mean`, etc.)
#' @param given A vector that can be ordered; the reference distribution X that
#' Y will be correlated with.
#' @param rho A rank correlation coefficient between -1 and 1.
#'
#' @examples
#'
#' # Generate a variable of interest
#' exam_score <- pmin(100, rnorm(n = 100, mean = 80, sd = 10))
#'
#' # Generate a correlated variable using fabricatr variable generation
#' scholarship_offers <- correlate(given = exam_score, rho = 0.7,
#'                                 draw_count, mean = 3)
#'
#' # Generate a correlated variable using base R distributions
#' final_grade <- pmax(100, correlate(given = exam_score, rho = 0.7,
#'                                    rnorm, mean = 80, sd = 10))
#'
#' @importFrom stats ecdf qnorm pnorm
#' @importFrom rlang is_closure
#' @export
correlate <- function(draw_handler, ..., given, rho) {
  # Error handling
  if(!is.numeric(rho)) {
    stop("`rho` used for correlated variable draws must be numeric.")
  }
  if(length(rho) > 1) {
    stop("`rho` used for correlated variable draws must be a single number.")
  }
  if(rho < -1 || rho > 1) {
    stop("`rho` used for correlated variables must be between -1 and 1 ",
         "inclusive.")
  }
  if(!is.null(dim(given))) {
    stop("`x` used for correlated variables must be a single vector.")
  }
  if(is.null(given)) {
    stop("`x` used for correlated variables must not be null.")
  }
  if(!is_closure(draw_handler)) {
    stop("You must pass a `draw_*` function to correlate as the first ",
         "argument.")
  }

  # Strategy here is to use affine transformation to make X to Standard Normal
  # X -> ECDF -> Quantile X -> INV CDF Std. Nor. -> Standard Normal X
  std_normal_base <- qnorm(
    rank(given) / (length(given) + 1)
  )
  # Why do we use rank(x) / ... and not ecdf(x)(x)?
  # ecdf(x)(x) will give some item the quantile 1, which will given an infinite
  # z-score. This prevents that. rank's default tie-breaker is "average",
  # which ensures two inputs with the same value have the same conditional mean
  # in the conditional Y distro.

  # Std. Normal X -> Std. Normal Y
  # Known conditional distribution of Y on X;
  # because X and Y will both be mean 0 var/sd 1, we know the formula will be
  # Y ~ Normal(rho * X, (1 - rho^2))
  std_normal_y <- rnorm(length(given),
                        rho * std_normal_base,
                        sqrt(1 - rho^2))

  # Std. Normal Y -> CDF -> Quantile Y.
  # Outer function handles Quantile Y -> Distribution Y
  quantile_y <- pnorm(std_normal_y)

  # User passed a well-behaved function -- just hand through
  if("quantile_y" %in% names(formals(draw_handler))) {
    return(draw_handler(..., quantile_y = quantile_y))
  }
#
  # Now check if this is a function for random number generation for the
  # base functions -- if so, replace with the quantile function
  new_draw_handler <- lookup_quantile_function(draw_handler)

  # Valid function
  if(is.function(new_draw_handler)) {
    return(new_draw_handler(p = quantile_y, ...))
  }

  # Error if the user provides a poorly specified function.
  stop("The draw_handler argument for a `correlate` call must be the name ",
       "of a base R distribution number generator (e.g. rnorm, runif, rpois)",
       " or the name of a fabricatr variable generating function (e.g. ",
       "`draw_count`, `draw_binomial`) or the name of a custom function ",
       "that contains a `quantile_y` argument.")
}


#' @importFrom stats rbeta rbinom rcauchy rchisq rexp rf rgamma rgeom rhyper
#' @importFrom stats rlnorm rnbinom rnorm rpois rt runif rweibull
#' @importFrom stats qbeta qbinom qcauchy qchisq qexp qf qgamma qgeom qhyper
#' @importFrom stats qlnorm qnbinom qnorm qpois qt qunif qweibull
lookup_quantile_function <- local({
    # Map from r to q functions.
    r_funs <- list(rbeta, rbinom, rcauchy, rchisq,
                   rexp, rf, rgamma, rgeom, rhyper,
                   rlnorm, rnbinom, rnorm, rpois, rt,
                   runif, rweibull)
    q_funs <- list(qbeta, qbinom, qcauchy, qchisq,
                   qexp, qf, qgamma, qgeom, qhyper,
                   qlnorm, qnbinom, qnorm, qpois, qt,
                   qunif, qweibull)

    lookup <- function(list_of_f, f) {
      which(vapply(list_of_f, identical, FALSE, f))
    }

    function(func_handler) {
      # If we're an r* function...
      index_match <- lookup(r_funs, func_handler)

      if(length(index_match) == 0) {
          index_match <- lookup(q_funs, func_handler)
          if(length(index_match) == 0) {
              return(NULL)
          }
      }
      q_funs[[index_match[1]]]

    }
})

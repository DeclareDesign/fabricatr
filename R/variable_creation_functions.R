#' Draw discrete variables including binary, binomial count, poisson count,
#' ordered, and categorical
#'
#' Drawing discrete data based on probabilities or latent traits is a common
#' task that can be cumbersome. Each function in our discrete drawing set creates
#' a different type of discrete data: \code{draw_binary} creates binary 0/1 data,
#' \code{draw_binomial} creates binomial data (repeated trial binary data),
#' \code{draw_categorical} creates categorical data, \code{draw_ordered}
#' transforms latent data into observed ordered categories, \code{draw_count}
#' creates count data (poisson-distributed). \code{draw_likert} is an alias to
#' \code{draw_ordered} that pre-specifies break labels and offers default breaks
#' appropriate for a likert survey question.
#'
#' For variables with intra-cluster correlations, see
#' \code{\link{draw_binary_icc}} and \code{\link{draw_normal_icc}}
#'
#' @param prob A number or vector of numbers representing the probability for
#' binary or binomial outcomes; or a number, vector, or matrix of numbers
#' representing probabilities for categorical outcomes. If you supply a link
#' function, these underlying probabilities will be transformed.
#' @param trials for `draw_binomial`, the number of trials for each observation
#' @param mean for `draw_count`, the mean number of count units for each observation
#' @param x for `draw_ordered` or `draw_likert`, the latent data for each
#' observation.
#' @param breaks vector of breaks to cut a latent outcome into ordered
#' categories with `draw_ordered` or `draw_likert`
#' @param break_labels vector of labels for the breaks to cut a latent outcome
#' into ordered categories with `draw_ordered`. (Optional)
#' @param category_labels vector of labels for the categories produced by
#' `draw_categorical`. If provided, must be equal to the number of categories
#' provided in the `prob` argument.
#' @param type Type of Likert scale data for `draw_likert`. Valid options are 4,
#' 5, and 7. Type corresponds to the number of categories in the Likert scale.
#' @param N number of units to draw. Defaults to the length of the vector of
#' probabilities or latent data you provided.
#' @param link link function between the latent variable and the probability of
#' a postiive outcome, e.g. "logit", "probit", or "identity". For the "identity"
#' link, the latent variable must be a probability.
#' @param quantile_y Ignore this for now.
#' @return A vector of data in accordance with the specification; generally
#' numeric but for some functions, including `draw_ordered`, may be factor if
#' break labels are provided.
#' @examples
#'
#' # Drawing binary values (success or failure, treatment assignment)
#' fabricate(N = 3,
#'    p = c(0, .5, 1),
#'    binary = draw_binary(prob = p))
#'
#' # Drawing binary values with probit link (transforming continuous data
#' # into a probability range).
#' fabricate(N = 3,
#'    x = 10 * rnorm(N),
#'    binary = draw_binary(prob = x, link = "probit"))
#'
#' # Repeated trials: `draw_binomial`
#' fabricate(N = 3,
#'    p = c(0, .5, 1),
#'    binomial = draw_binomial(prob = p, trials = 10))
#'
#' # Ordered data: transforming latent data into observed, ordinal data.
#' # useful for survey responses.
#' fabricate(N = 3,
#'    x = 5 * rnorm(N),
#'    ordered = draw_ordered(x = x,
#'                           breaks = c(-Inf, -1, 1, Inf)))
#'
#' # Providing break labels for latent data.
#' fabricate(N = 3,
#'    x = 5 * rnorm(N),
#'    ordered = draw_ordered(x = x,
#'                           breaks = c(-Inf, -1, 1, Inf),
#'                           break_labels = c("Not at all concerned",
#'                                            "Somewhat concerned",
#'                                            "Very concerned")))
#'
#' # Likert data: often used for survey data
#' fabricate(N = 10,
#'           support_free_college = draw_likert(x = rnorm(N),
#'                                              type = 5))
#'
#' # Count data: useful for rates of occurrences over time.
#' fabricate(N = 5,
#'    x = c(0, 5, 25, 50, 100),
#'    theft_rate = draw_count(mean=x))
#'
#' # Categorical data: useful for demographic data.
#' fabricate(N = 6, p1 = runif(N), p2 = runif(N), p3 = runif(N),
#'           cat = draw_categorical(cbind(p1, p2, p3)))
#'
#' @importFrom stats pnorm rnorm rpois rbinom na.omit qbinom qpois
#' @export
#'
draw_binomial <- function(prob, trials=1, N = length(prob), link = "identity",
                          quantile_y = NULL) {
  # Error handle probabilities and apply link function.
  if (mode(prob) != "numeric") {
    stop("Probabilities provided in the `prob` argument must be numeric.")
  }

  if (link == "identity") {
    if (!all(na.omit(0 <= prob & prob <= 1))) {
      stop(
        "The identity link requires probability values between 0 and 1,",
        "inclusive."
      )
    } else if (any(is.na(prob))) {
      warning("At least one specified probability (`prob`) was NA.")
    }
    if (N %% length(prob)) {
      stop(
        "`N` is not an even multiple of the length of the number of
        probabilities, `prob`."
      )
    }
  } else if (link == "logit") {
    prob <- 1 / (1 + exp(-prob))
  } else if (link == "probit") {
    prob <- pnorm(prob)
  } else {
    stop(
      "Only 'identity', 'logit', 'and 'probit' are valid link functions for ",
      "`draw_binomial()` and `draw_binary()`."
    )
  }

  # Error handle trials
  if (is.vector(trials) && length(trials) > 1) {
    if (N %% length(trials) != 0) {
      stop(
        "`N` is not an even multiple of the length of the number of
        trials, `trials`."
      )
    }
    if (!is.integer(trials) && is.numeric(trials) && any(trials %% 1 != 0)) {
      stop(
        "All numbers of trials should be integer numbers."
      )
    }
  }
  if (!is.null(dim(trials))) {
    stop(
      "Number of trials must be an integer or vector, not higher-dimensional."
    )
  }
  if (is.null(trials) || any(is.na(trials))) {
    stop(
      "Number of trials must be specified, not null or NA."
    )
  }
  if (!is.integer(trials) && is.numeric(trials) && any(trials %% 1 != 0)) {
    stop(
      "Number of trials must be an integer."
    )
  }
  if (any(trials <= 0)) {
    stop(
      "Number of trials must be a positive integer."
    )
  }

  # Prob and trials must be single numbers if quantile_y is provided
  if(!is.null(quantile_y) && (length(prob) > 1 || length(trials) > 1)) {
    stop(
      "When generating a correlated binary or binomial random variable, the ",
      "`prob` and `trials` arguments must be single numbers and not a ",
      "function of other variables."
    )
  }

  if(is.null(quantile_y)) {
    return(rbinom(N, trials, prob))
  } else {
    return(qbinom(quantile_y, trials, prob))
  }
}

#' @rdname draw_binomial
#' @export
draw_categorical <- function(prob, N = NULL, link = "identity",
                             category_labels = NULL) {
  if (link != "identity") {
    stop("Categorical data does not accept link functions.")
  }

  if (is.null(dim(prob))) {
    if (is.vector(prob) && is.numeric(prob) && length(prob) > 1) {
      if (is.null(N)) {
        stop(
          "If `prob` is a vector of category probabilities, you must provide ",
          "an explicit `N` argument."
        )
      }
      prob <- matrix(rep(prob, N), byrow = TRUE, ncol = length(prob), nrow = N)
      message(
        "For a categorical (multinomial) distribution, a matrix of ",
        "probabilities should be provided. The data below is generated by ",
        "interpreting the vector of category probabilities you provided as ",
        "identical for each observation."
      )
    } else {
      stop(
        "For a categorical (multinomial) distribution, a matrix of ",
        "probabilities must be provided"
      )
    }
  }
  if (!all(apply(prob, 1, min) > 0)) {
    stop(
      "For a categorical (multinomial) distribution, the elements of `prob` ",
      "should be positive and sum to a positive number."
    )
  }

  if (is.null(N)) {
    N <- nrow(prob)
  }

  if (!(nrow(prob) %in% c(1, N))) {
    stop("The number of probabilities provided should be equal to `N` or 1.")
  }

  if(!is.null(category_labels) && length(category_labels) != ncol(prob)) {
    stop("If provided, the number of category labels (",
         length(category_labels),
         ") must equal the number of categories. (", ncol(prob), ")")
  }

  m <- ncol(prob)
  rcateg <- function(p)
    sample(1:m, 1, prob = p)

  draws <- apply(prob, 1, rcateg)

  if(!is.null(category_labels)) {
    return(category_labels[draws])
  } else {
    return(draws)
  }
}

#' @rdname draw_binomial
#' @export
draw_ordered <- function(x, breaks = c(-1, 0, 1), break_labels = NULL,
                         N = length(x), link = "identity", quantile_y = NULL) {
  # Link function
  if (link == "probit") {
    x <- x + rnorm(N)
  } else if (link != "identity") {
    stop("`draw_ordered()` only accepts 'identity' and 'probit' link ",
         "functions.")
  }

  # Error handling breaks
  if (is.null(breaks) || any(is.na(breaks))) {
    stop("You must specify numeric breaks for ordered data.")
  }
  if (!is.numeric(breaks)) {
    stop("All breaks specified for ordered data must be numeric.")
  }
  if (is.matrix(breaks) || is.data.frame(breaks)) {
    stop("Numeric breaks must be a vector.")
  }
  if (is.unsorted(breaks)) {
    stop("Numeric breaks must be in ascending order.")
  }

  # Check N/x
  if (N %% length(x) != 0) {
    stop("`N` must be an even multiple of the length of `x`.")
  }

  # Pre-pend -Inf
  if (any(breaks[1] > x)) {
    breaks <- c(-Inf, breaks)
  }
  # Post-pend Inf
  if (any(breaks[length(breaks)] < x)) {
    breaks <- c(breaks, Inf)
  }

  # Make sure break labels are concordant with breaks.
  if (!is.null(break_labels) &&
    (is.vector(break_labels) &&
      !is.logical(break_labels) &&
      all(!is.na(break_labels)) &&
      length(break_labels) != length(breaks) - 1)) {
    stop(
      "Break labels should be of length one less than breaks. ",
      "Currently you have ", length(break_labels), " bucket labels and ",
      length(breaks) - 1, " buckets of data."
    )
  }

  # Output
  if (!is.null(break_labels)) {
    return(factor(
      break_labels[findInterval(x, breaks)],
      levels = break_labels
    ))
  } else {
    return(findInterval(x, breaks))
  }
}

#' @rdname draw_binomial
#' @export
draw_count <- function(mean, N = length(mean), link = "identity",
                       quantile_y = NULL) {
  if (link != "identity") {
    stop("Count data does not accept link functions.")
  }

  if (any(mean < 0)) {
    stop(
      "All provided count values must be non-negative."
    )
  }

  if (N %% length(mean) != 0) {
    stop("`N` must be an even multiple of the length of mean.")
  }

  # Prob and trials must be single numbers if quantile_y is provided
  if(!is.null(quantile_y) && length(mean) > 1) {
    stop(
      "When generating a correlated count variable, the `mean` argument must ",
      "be a single number and not a function of other variables."
    )
  }

  if(is.null(quantile_y)) {
    return(rpois(N, lambda = mean))
  } else {
    return(qpois(quantile_y, lambda = mean))
  }

}

#' @rdname draw_binomial
#' @export
draw_binary <- function(prob, N = length(prob), link = "identity",
                        quantile_y = NULL) {
  return(draw_binomial(
    prob,
    N = N,
    link = link,
    trials = 1,
    quantile_y = quantile_y
  ))
}

#' @rdname draw_binomial
#' @export
draw_likert <- function(x,
                        type = 7,
                        breaks = NULL,
                        N = length(x),
                        link = "identity",
                        quantile_y = NULL) {
  if (is.null(breaks) && is.null(type)) {
    stop("You must provide either `breaks` or `type` to a `draw_likert()` ",
         "call.")
  }

  if (is.null(breaks)) {
    if (type == 7) {
      breaks <- c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf)
    } else if (type == 5) {
      breaks <- c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf)
    } else if (type == 4) {
      breaks <- c(-Inf, -1, 0, 1, Inf)
    } else {
      stop("Valid `type` arguments for a `draw_likert()` call are 4, 5, and 7.")
    }
  }

  if (length(breaks) == 8) {
    break_labels <- c(
      "Strongly Disagree",
      "Disagree",
      "Lean Disagree",
      "Don't Know / Neutral",
      "Lean Agree",
      "Agree",
      "Strong Agree"
    )
  } else if (length(breaks) == 6) {
    break_labels <- c(
      "Strongly Disagree",
      "Disagree",
      "Don't Know / Neutral",
      "Agree",
      "Strongly Agree"
    )
  } else if (length(breaks) == 5) {
    break_labels <- c(
      "Strongly Disagree",
      "Disagree",
      "Agree",
      "Strongly Agree"
    )
  } else {
    stop(
      "If you provide `draw_likert()` with a `breaks` argument, `breaks` must ",
      "be either 5, 6, or 8 elements long for 4, 5, or 7 category Likert data."
    )
  }

  return(draw_ordered(
    x = x,
    breaks = breaks,
    N = N,
    link = link,
    break_labels = break_labels,
    quantile_y = quantile_y
  ))
}

#' @rdname draw_binomial
#' @importFrom stats runif
#' @export
draw_quantile <- function(type = NULL,
                          N = NULL) {

  if(is.null(N) || !is.numeric(N)) {
    stop("`N` must be provided to `draw_quantile()` and must be numeric.")
  }
  if(!is.null(dim(N)) || length(N) > 1) {
    stop("`N` must be a single number.")
  }
  if(N <= 0) {
    stop("`N` provided to `draw_quantile()` must be positive.")
  }

  if(is.null(type) || !is.numeric(type)) {
    stop("`type` must be provided to `draw_quantile()` and must be numeric.")
  }
  if(!is.null(dim(type)) || length(type) > 1) {
    stop("`type` must be a single number.")
  }
  if(type <= 1) {
    stop("`type` provided to `draw_quantile()` must be at least 2.")
  }
  if(type >= N) {
    stop("`type` provided to `draw_quantile()` must be less than `N`.")
  }

  latent_data <- runif(n = N, min = 0, max = 1)
  split_quantile(latent_data, type = type)
}

#' Split data into quantile buckets (e.g. terciles, quartiles, quantiles,
#' deciles).
#'
#' Survey data is often presented in aggregated, depersonalized form, which
#' can involve binning underlying data into quantile buckets; for example,
#' rather than reporting underlying income, a survey might report income by
#' decile. `split_quantile` can automatically produce this split using any
#' data `x` and any number of splits `type.
#'
#' @param x A vector of any type that can be ordered -- i.e. numeric or factor
#' where factor levels are ordered.
#' @param type The number of buckets to split data into. For a median split,
#' enter 2; for terciles, enter 3; for quartiles, enter 4; for quintiles, 5;
#' for deciles, 10.
#'
#' @examples
#'
#' # Divide this arbitrary data set in 3.
#' data_input <- rnorm(n = 100)
#' split_quantile(x = data_input, type = 3)
#'
#' @importFrom stats quantile
#' @export
split_quantile <- function(x = NULL,
                           type = NULL) {
  if(is.null(x) || length(x) < 2) {
    stop("The `x` argument provided to quantile split must be non-null and ",
         "length at least 2.")
  }
  if(is.null(type) || !is.numeric(type)) {
    stop("The `type` argument provided to quantile split must be non-null and ",
         "numeric.")
  }

  cut(x, breaks = quantile(x, probs = seq(0, 1, length.out = type + 1)),
      labels = 1:type,
      include.lowest = TRUE)
}

#' Perform generation of a correlated random variable.
#'
#' In order to generate a random variable of a specific distribution based on
#' another variable of any distribution and a correlation coefficient `rho`,
#' we map the first, known variable into the standard normal space via affine
#' transformation, and generate a conditional distribution of Y. This function
#'
#' @param draw_handler The unquoted name of a function to generate data.
#' Currently, `draw_binary`, `draw_binomial`, and `draw_count` are supported.
#' @param ... The arguments to draw_handler (e.g. `prob`, `mean`, etc.)
#' @param given A vector that can be ordered; the reference distribution X that
#' Y will be correlated with.
#' @param rho A rank correlation coefficient between -1 and 1.
#'
#' @importFrom stats ecdf qnorm rnorm pnorm
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
  std_normal_base <- qnorm(ecdf(given)(given))

  # Std. Normal X -> Std. Normal Y
  # Known conditional distribution of Y on X;
  # because X and Y will both be mean 0 var/sd 1, we know the formula will be
  # Y ~ Normal(rho * X, (1 - rho^2))
  std_normal_y <- rho * std_normal_base + rnorm(length(given),
                                                0,
                                                sqrt(1 - rho^2))

  # Std. Normal Y -> CDF -> Quantile Y.
  # Outer function handles Quantile Y -> Distribution Y
  quantile_y <- pnorm(std_normal_y)

  # Pass quantile to the provided function with passthrough arguments
  draw_handler(..., quantile_y = quantile_y)
}

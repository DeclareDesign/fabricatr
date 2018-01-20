#' Draw normal data with fixed intra-cluster correlation.
#'
#' Data is generated to ensure inter-cluster correlation 0, intra-cluster
#' correlation in expectation ICC. The data generating process
#' used in this function is specified at the following URL:
#' \url{https://stats.stackexchange.com/questions/263451/create-synthetic-data-with-a-given-intraclass-correlation-coefficient-icc}
#'
#' The typical use for this function is for a user to provide an \code{ICC} and,
#' optionally, a set of within-cluster standard deviations, \code{sd}. If the
#' user does not provide \code{sd}, the default value is 1. These arguments
#' imply a fixed between-cluster standard deviation.
#'
#' An alternate mode for the function is to provide between-cluster standard
#' deviations, \code{sd_between}, and an \code{ICC}. These arguments imply
#' a fixed within-cluster standard deviation.
#'
#' If users provide all three of \code{ICC}, \code{sd_between}, and
#' \code{sd}, the function will warn the user and use the provided standard
#' deviations for generating the data.
#'
#' @param x A number or vector of numbers, one mean per cluster. If none is
#' provided, will default to 0.
#' @param N (Optional) A number indicating the number of observations to be
#' generated. Must be equal to length(clusters) if provided.
#' @param clusters A vector of factors or items that can be coerced to
#' clusters; the length will determine the length of the generated data.
#' @param sd A number or vector of numbers, indicating the standard deviation of
#' each cluster's error terms -- standard deviation within a cluster (default 1)
#' @param sd_between A number or vector of numbers, indicating the standard deviation
#' between clusters.
#' @param ICC A number indicating the desired ICC.
#' @return A vector of numbers corresponding to the observations from
#' the supplied cluster IDs.
#' @examples
#' clusters = rep(1:5, 10)
#' draw_normal_icc(clusters = clusters, ICC = 0.5)
#'
#' @importFrom stats rnorm
#'
#' @export
draw_normal_icc = function(x = 0,
                           N = NULL,
                           clusters,
                           sd = NULL,
                           sd_between = NULL,
                           ICC = NULL) {

  # Let's not worry about how clusters are provided
  tryCatch({
    clusters = as.numeric(as.factor(clusters))
  }, error=function(e) {
    stop("Error coercing cluster IDs to factor levels.")
  })
  number_of_clusters = length(unique(clusters))

  # Sanity check N
  if(!is.null(N) && !is.numeric(N)) {
    stop("If you provide an N, it must be numeric.")
  }
  if(!is.null(N) && N != length(clusters)) {
    stop("If you provide an N, it must be equal to the length of provided ",
         "cluster ids")
  }

  # Sanity check x
  if(!is.vector(x)) {
    stop("x must be a number or vector of numbers.")
  }
  if(!length(x) %in% c(1, number_of_clusters, length(clusters))) {
    stop("x must be either one number or one number per cluster.")
  }
  if(length(x) == length(clusters) &&
     nrow(unique(cbind(x, clusters))) != number_of_clusters) {
    stop("If x is provided for each observation, it must be unique per cluster.")
  }
  if(any(!is.numeric(x))) {
    stop("x must be a number or vector of numbers.")
  }

  # Sanity check ICC
  if(is.null(ICC)) {
    if(is.null(sd) | is.null(sd_between)) {
      stop("If ICC is not provided, both sd and sd_between must be provided.")
    } else {
      implied_ICC = sd_between^2 / (sd_between^2 + sd^2)
      message("Implied ICC of provided standard deviations: ", implied_ICC)
    }
  } else {
    if(length(ICC) > 1) {
      stop("ICC must be a single number.")
    }
    if(!is.numeric(ICC)) {
      stop("ICC must be a number.")
    }
    if(ICC > 1 | ICC < 0) {
      stop("ICC must be a number between 0 and 1.")
    }
    if(ICC >= 0.999 & !is.null(sd)) {
      stop("ICC of 1 with a finite within-cluster variance requires division ",
           "by zero to infer between-cluster variance. Try lower ICC or specify ",
           "between- and within-cluster variance to infer ICC.")
    }
    if(ICC <= 0.001 & !is.null(sd)) {
      stop("ICC of 0 with a finite within-cluster variance implies zero ",
           "between-cluster variance. You can generate data with zero ICC ",
           "using R's standard rnorm command to generate normal data independent ",
           "of cluster variable.")
    }
    if(ICC <= 0.001 & !is.null(sd_between)) {
      stop("ICC of 0 with a finite between-cluster variance requires division ",
           "by zero to infer within-cluster variance. Try higher ICC or specify ",
           "between- and within-cluster variance to infer ICC.")
    }
    if(ICC >= 0.999 & !is.null(sd_between)) {
      stop("ICC of 1 with a finite between-cluster variance implies zero ",
           "within-cluster variance. You can generate data that is static per ",
           "cluster using fabricatr's nested level functionality using a two-level ",
           "design where the outer level is cluster and the inner level is observation.")
    }


    if(!is.null(sd) & !is.null(sd_between)) {
      implied_ICC = sd_between^2 / (sd_between^2 + sd^2)
      warning("Providing both between-cluster and within-cluster standard ",
              "deviations implies an ICC of ", implied_ICC, ". Ignoring provided",
              " ICC")
    }

    if(is.null(sd) & is.null(sd_between)) {
      sd = 1
    }
  }

  # Sanity check sd
  if(!is.null(sd)) {
    if(!length(sd) %in% c(1, number_of_clusters)) {
      stop("sd must be either a number or one number per cluster.")
    }
    if(!is.vector(sd)) {
      stop("sd must be a number or vector of numbers.")
    }
    if(any(!is.numeric(sd))) {
      stop("sd must be a number or vector of numbers.")
    }
    if(any(sd < 0)) {
      stop("Numbers provided to sd must be non-negative.")
    }
  }

  # Sanity check sd_between
  if(!is.null(sd_between)) {
    if(!length(sd_between) %in% c(1, number_of_clusters)) {
      stop("sd_between, if provided, must be either a number or one number per cluster.")
    }
    if(!is.vector(sd_between)) {
      stop("sd_between, if provided, must be a number or vector of numbers.")
    }
    if(any(!is.numeric(sd))) {
      stop("sd_between, if provided, must be a number or vector of numbers.")
    }
    if(any(sd_between < 0)) {
      stop("Numbers provided to sd_between must be non-negative.")
    }
  }

  # Get number of clusters
  number_of_clusters = length(unique(clusters))

  # Fill in the unfilled number at this point.
  if(is.null(sd)) {
    sd = sqrt( ((1 - ICC) * sd_between^2) / ICC )
  }
  if(is.null(sd_between)) {
    sd_between = sqrt( (ICC * sd^2) / (1 - ICC) )
  }

  # Cluster means are either the same or individually supplied
  if(length(x) == 1) {
    cluster_mean = rep(x, number_of_clusters)
  } else {
    cluster_mean = x
  }

  # Each individual has a realization of their cluster's mean
  individual_mean = cluster_mean[clusters]

  # Cluster level draws, expanded to individual level draws
  alpha_cluster = rnorm(n=number_of_clusters,
                        mean=0,
                        sd=sd_between)
  alpha_individual = alpha_cluster[clusters]

  # And error terms, which are truly individual
  epsilon_ij = rnorm(length(clusters), 0, sd)

  individual_mean + alpha_individual + epsilon_ij
}

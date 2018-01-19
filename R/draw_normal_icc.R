#' Draw normal data with fixed intra-cluster correlation.
#'
#' Data is generated to ensure inter-cluster correlation 0, intra-cluster
#' correlation in expectation ICC. The data generating process
#' used in this function is specified at the following URL:
#' \url{https://stats.stackexchange.com/questions/263451/create-synthetic-data-with-a-given-intraclass-correlation-coefficient-icc}
#'
#' @param x A number or vector of numbers, one mean per cluster. If none is
#' provided, will default to 0.
#' @param N (Optional) A number indicating the number of observations to be
#' generated. Must be equal to length(clusters) if provided.
#' @param clusters A vector of factors or items that can be coerced to
#' clusters; the length will determine the length of the generated data.
#' @param sd A number or vector of numbers, indicating the standard deviation of
#' each cluster's error terms
#' @param ICC A number indicating the desired ICC. If none is provided,
#' will default to 0.
#' @return A vector of numbers corresponding to the observations from
#' the supplied cluster IDs.
#' @examples
#' clusters = rep(1:5, 10)
#' draw_normal_icc(clusters = clusters)
#'
#' @importFrom stats rnorm
#'
#' @export
draw_normal_icc = function(x = 0,
                           N = NULL,
                           clusters,
                           sd = 1,
                           ICC = 0) {
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
  if(length(ICC) > 1) {
    stop("ICC must be a single number.")
  }
  if(!is.numeric(ICC)) {
    stop("ICC must be a number.")
  }
  if(ICC > 1 | ICC < 0) {
    stop("ICC must be a number between 0 and 1.")
  }

  # Sanity check sd
  if(!length(sd) %in% c(1, number_of_clusters)) {
    stop("sd must be either a number or one number per cluster.")
  }
  if(!is.vector(sd)) {
    stop("sd must be a number or vector of numbers.")
  }
  if(any(!is.numeric(sd))) {
    stop("sd must be a number or vector of numbers.")
  }

  # Get number of clusters
  number_of_clusters = length(unique(clusters))
  # Convert ICC to implied variance per cluster
  recover_var_cluster = (ICC * sd^2) / (1 - ICC)

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
                        sd=sqrt(recover_var_cluster))
  alpha_individual = alpha_cluster[clusters]

  # And error terms, which are truly individual
  epsilon_ij = rnorm(length(clusters), 0, sd)

  individual_mean + alpha_individual + epsilon_ij
}

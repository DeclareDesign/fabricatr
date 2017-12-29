#' Draw binary data with fixed intra-cluster correlation.
#'
#' Data is generated to ensure inter-cluster correlation 0, intra-cluster
#' correlation in expectation \eqn{\rho}. Algorithm taken from Hossein,
#' Akhtar. "ICCbin: An R Package Facilitating Clustered Binary Data
#' Generation, and Estimation of Intracluster Correlation Coefficient (ICC)
#' for Binary Data".
#'
#' @param x A number or vector of numbers, one probability per cluster. If none
#' is provided, will default to 0.5.
#' @param N (Optional) A number indicating the number of observations to be
#' generated. Must be equal to length(clusters) if provided.
#' @param clusters A vector of factors or items that can be coerced to
#' clusters; the length will determine the length of the generated data.
#' @param rho A number indicating the desired ICC, if none is provided will
#' default to 0.
#' @return A vector of binary numbers corresponding to the observations from
#' the supplied cluster IDs.
#' @examples
#' clusters = rep(1:5, 10)
#' draw_binary_icc(clusters = clusters)
#' draw_binary_icc(x = 0.5, clusters = clusters, rho = 0.5)
#'
#' @importFrom stats rbinom
#'
#' @export
draw_binary_icc = function(x = 0.5, N = NULL, clusters, rho = 0) {
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
  if(is.null(N)) {
    N = length(x)
  }

  # Sanity check x
  if(!length(x) %in% c(1, number_of_clusters, N)) {
    stop("x must be either one number or one number per cluster.")
  }
  if(length(x) == N && nrow(unique(cbind(x, clusters))) != number_of_clusters) {
    stop("If x is provided for each observation, it must be unique per cluster.")
  }

  if(!is.vector(x)) {
    stop("x must be a number or vector of numbers.")
  }
  if(any(!is.numeric(x))) {
    stop("x must be a number or vector of numbers.")
  }
  if(any(x > 1 | x < 0)) {
    stop("x must be numeric probabilities between 0 and 1 inclusive.")
  }

  # Sanity check rho
  if(length(rho) > 1) {
    stop("rho must be a single number.")
  }
  if(!is.numeric(rho)) {
    stop("rho must be a number.")
  }
  if(rho > 1 | rho < 0) {
    stop("rho must be a number between 0 and 1.")
  }

  # Generate cluster and individual probabilities
  if(length(x) == 1) {
    cluster_prob = rep(x, number_of_clusters)
  } else {
    cluster_prob = x
  }
  # Individual probabilities: subset operator maps cluster probs to units.
  individual_prob = cluster_prob[clusters]

  # Draw the z_ijs
  cluster_draw = rbinom(n = number_of_clusters,
                        size = 1,
                        prob = cluster_prob)[clusters]

  # Draw the y_ijs
  individual_draw = rbinom(n = length(clusters),
                           size = 1,
                           prob = individual_prob)

  # Draw the u_ijs -- sqrt(rho) because the actual ICC for this data will be
  # rho^2 -- sqrt(rho^2) = rho, to ensure users can enter in the terms they feel
  # most comfortable in
  switch_draw = rbinom(n = length(clusters),
                       size = 1,
                       prob = sqrt(rho))

  # Return either the cluster outcome or individual outcome depending on the
  # switch
  ifelse(switch_draw, cluster_draw, individual_draw)
  }

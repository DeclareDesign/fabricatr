#' Draw binary data with fixed intra-cluster correlation.
#'
#' Data is generated according to the following algorithm, where \eqn{i} is
#' the index of a cluster and \eqn{j} is the index of a unit: \deqn{z_i ~
#' Bernoulli(p_i) \cr
#' y_{ij} ~ Bernoulli(p_{ij}) \cr
#' u_{ij} ~ Bernoulli(sqrt(\rho)) \cr
#' x_{ij} = (u_{ij}) z_i + (1 - u_{ij}) y_{ij}}
#'
#' This system of equations ensures inter-cluster correlation 0, intra-cluster
#' correlation in expectation \eqn{\rho}. Algorithm from Hossein, Akhtar.
#' "ICCbin: An R Package Facilitating Clustered Binary Data Generation, and
#' Estimation of Intracluster Correlation Coefficient (ICC) for Binary Data".
#' We rederived the analytical properties of this data and did a simulation
#' study to confirm that the data generated ensured the ICC we mentioned.
#'
#' @param x A number or vector of numbers, one probability per cluster.
#' @param N (Optional) A number indicating the number of observations to be
#' generated. Must be equal to length(cluster_ids) if provided.
#' @param cluster_ids A vector of factors or items that can be coerced to
#' clusters; the length will determine the length of the generated data.
#' @param rho A number indicating the desired RCC.
#' @return A vector of binary numbers corresponding to the observations from
#' the supplied cluster IDs.
#' @examples
#' cluster_ids = rep(1:5, 10)
#' draw_binary_icc(cluster_ids = cluster_ids)
#' draw_binary_icc(x = 0.5, cluster_ids = cluster_ids, rho = 0.5)
#'
#' @importFrom stats rbinom
#'
#' @export
draw_binary_icc = function(x = 0.5, N = NULL, cluster_ids, rho = 0.5) {
  # Let's not worry about how cluster_ids are provided
  tryCatch({
    cluster_ids = as.numeric(as.factor(cluster_ids))
  }, error=function(e) {
    stop("Error coercing cluster IDs to factor levels.")
  })
  number_of_clusters = length(unique(cluster_ids))

  # Sanity check x
  if(!length(x) %in% c(1, number_of_clusters)) {
    stop("x must be either one number or one number per cluster.")
  }
  if(!is.null(N) && !is.numeric(N)) {
    stop("If you provide an N, it must be numeric.")
  }
  if(!is.null(N) && N != length(cluster_ids)) {
    stop("If you provide an N, it must be equal to the length of provided
         cluster ids")
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
  individual_prob = cluster_prob[cluster_ids]

  # Draw the z_ijs
  cluster_draw = rbinom(n = number_of_clusters,
                        size = 1,
                        prob = cluster_prob)[cluster_ids]

  # Draw the y_ijs
  individual_draw = rbinom(n = length(cluster_ids),
                           size = 1,
                           prob = individual_prob)

  # Draw the u_ijs -- sqrt(rho) because the actual ICC for this data will be
  # rho^2 -- sqrt(rho^2) = rho, to ensure users can enter in the terms they feel
  # most comfortable in
  switch_draw = rbinom(n = length(cluster_ids),
                       size = 1,
                       prob = sqrt(rho))

  # Return either the cluster outcome or individual outcome depending on the
  # switch
  ifelse(switch_draw, cluster_draw, individual_draw)
  }

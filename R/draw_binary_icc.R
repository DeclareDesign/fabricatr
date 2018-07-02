#' Draw binary data with fixed intra-cluster correlation.
#'
#' Data is generated to ensure inter-cluster correlation 0, intra-cluster
#' correlation in expectation ICC. Algorithm taken from Hossein,
#' Akhtar. "ICCbin: An R Package Facilitating Clustered Binary Data
#' Generation, and Estimation of Intracluster Correlation Coefficient (ICC)
#' for Binary Data".
#'
#' @param prob A number or vector of numbers, one probability per cluster. If none
#' is provided, will default to 0.5.
#' @param N (Optional) A number indicating the number of observations to be
#' generated. Must be equal to length(clusters) if provided.
#' @param clusters A vector of factors or items that can be coerced to
#' clusters; the length will determine the length of the generated data.
#' @param ICC A number indicating the desired \code{ICC}, if none is provided
#' the default ICC will be 0.
#' @return A vector of binary numbers corresponding to the observations from
#' the supplied cluster IDs.
#' @examples
#' # Divide units into clusters
#' clusters = rep(1:5, 10)
#'
#' # Default probability 0.5, default ICC 0
#' draw_binary_icc(clusters = clusters)
#'
#' # Specify probability or ICC
#' corr_draw = draw_binary_icc(prob = 0.5, clusters = clusters, ICC = 0.5)
#'
#' # Verify ICC of data.
#' summary(lm(corr_draw ~ as.factor(clusters)))$r.squared
#'
#' @importFrom stats rbinom
#'
#' @export
draw_binary_icc <- function(prob = 0.5, N = NULL, clusters, ICC = 0) {
  check_draw_icc_args (clusters, N, prob, ICC);

  uclusters <- sort(unique(clusters));
  clusters <- match(clusters, uclusters)

  N <- length(clusters)
  k <- length(uclusters)


  # Generate cluster and individual probabilities
  cluster_prob <- if (length(prob) == 1) rep(prob, k) else prob
  individual_prob <- cluster_prob[clusters]

  # Draw the z_ijs
  z_i <- rbinom(n = k, size = 1, prob = cluster_prob)[clusters]

  # Draw the y_ijs
  y_i <- rbinom(n = N, size = 1,prob = individual_prob)

  # Draw the u_ijs -- sqrt(ICC) because the actual ICC for this data will be
  # ICC^2 -- sqrt(ICC^2) = ICC, to ensure users can enter in the terms they feel
  # most comfortable in
  u_i <- rbinom(n = N,size = 1,prob = sqrt(ICC))

  # Return either the cluster outcome or individual outcome depending on the
  # switch
  ifelse(u_i, z_i, y_i)
}


check_draw_icc_args <- function(clusters, N, prob, ICC) {
  if(!is_vector(clusters)) {
    stop("`clusters` must be a vector.")
  }

  # Sanity check N
  if (!is.null(N)) {
    if(N != length(clusters))
      stop(
        "If you provide an N for `draw_binary_icc()`, it must be equal to the length of provided cluster ids"
      )
  }

  k <- length(unique(clusters))

  # Sanity check prob
  if (!is.vector(prob) || any(prob > 1 | prob < 0)  ||
      !length(prob) %in% c(1, k, length(clusters)) ) {
    stop("`prob` must be a number or vector of numbers between 0 and 1, of length 1, k or n.")
  }

  if (length(prob) == length(clusters) && nrow(unique(cbind(prob, clusters))) != k) {
    stop("If `prob` is provided for each observation, it must be unique per cluster.")
  }

  # Sanity check ICC
  if (length(ICC) > 1 || !is.numeric(ICC) || ICC > 1 || ICC < 0) {
    stop("ICC must be a single number between 0 and 1.")
  }
}

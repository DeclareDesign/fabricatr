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

    if(is.null(clusters)) {
    stop("You must provide clusters to `draw_binary_icc`")
  }

  # Let's not worry about how clusters are provided
  if(!is.null(dim(clusters))) {
    stop(
      "You must provide cluster IDs for draw_normal_icc as a vector, not a ",
      "higher dimensional object like a data frame or similar."
    )
  }

  tryCatch({
    clusters <- as.numeric(as.factor(clusters))
  }, error = function(e) {
    stop(
      "Error coercing cluster IDs to factor levels. Please ensure the ",
      "`clusters` argument is numeric, factor, or can be coerced into being ",
      "a factor."
    )
  })

  number_of_clusters <- length(unique(clusters))

  # Sanity check N
  if (!is.null(N) && !is.numeric(N)) {
    stop("If you provide an N for `draw_binary_icc()`, it must be numeric.")
  }
  if (!is.null(N) && N != length(clusters)) {
    stop(
      "If you provide an N for `draw_binary_icc()`, it must be equal to the ",
      "length of provided cluster ids"
    )
  }

  # Sanity check prob
  if (!is.vector(prob)) {
    stop("`prob` must be a number or vector of numbers.")
  }
  if (!length(prob) %in% c(1, number_of_clusters, length(clusters))) {
    stop("`prob` must be either one number or one number per cluster.")
  }
  if (length(prob) == length(clusters) &&
    nrow(unique(cbind(prob, clusters))) != number_of_clusters) {
    stop("If `prob` is provided for each observation, it must be unique per ",
         "cluster.")
  }

  if (any(!is.numeric(prob))) {
    stop("`prob` must be a number or vector of numbers.")
  }
  if (any(prob > 1 | prob < 0)) {
    stop("`prob` must be numeric probabilities between 0 and 1 inclusive.")
  }

  # Sanity check ICC
  if (length(ICC) > 1) {
    stop("The ICC provided to `draw_binary_icc()` must be a single number.")
  }
  if (!is.numeric(ICC)) {
    stop("The ICC provided to `draw_binary_icc()` must be a number.")
  }
  if (ICC > 1 | ICC < 0) {
    stop("The ICC provided to `draw_binary_icc()` must be a number between 0 ",
         "and 1.")
  }

  # Generate cluster and individual probabilities
  if (length(prob) == 1) {
    cluster_prob <- rep(prob, number_of_clusters)
  } else {
    cluster_prob <- prob
  }
  # Individual probabilities: subset operator maps cluster probs to units.
  individual_prob <- cluster_prob[clusters]

  # Draw the z_ijs
  cluster_draw <- rbinom(
    n = number_of_clusters,
    size = 1,
    prob = cluster_prob
  )[clusters]

  # Draw the y_ijs
  individual_draw <- rbinom(
    n = length(clusters),
    size = 1,
    prob = individual_prob
  )

  # Draw the u_ijs -- sqrt(ICC) because the actual ICC for this data will be
  # ICC^2 -- sqrt(ICC^2) = ICC, to ensure users can enter in the terms they feel
  # most comfortable in
  switch_draw <- rbinom(
    n = length(clusters),
    size = 1,
    prob = sqrt(ICC)
  )

  # Return either the cluster outcome or individual outcome depending on the
  # switch
  ifelse(switch_draw, cluster_draw, individual_draw)
}

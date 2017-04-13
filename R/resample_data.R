#' Resample data, including multi-level data
#'
#' @param data A data.frame, usually provided by the user.
#' @param N The number of sample N to return. If multiple levels are defined in ID_labels, N should be a vector.
#' @param ID_labels The variables that indicate the data hierarchy.
#'
#' @return A data.frame
#'
#' @export
resample_data <- function(data, N, ID_labels = NULL) {

  if (missing(N)) {
    N <- nrow(data)
  }

  N_total <- prod(N)
  k <- length(N) ## number of levels

  # ID_labels <- substitute(ID_levels)
  # if (!is.null(ID_labels)) {
  #   ID_labels <- as.character(ID_labels)
  # }

  if (k != length(ID_labels)) {
    stop("If you provide more than one ID_labels to bootstrap for multilevel data, please provide a vector for N of the same length representing the number to resample at each level.")
  }

  sample_by_level <- list()
  for (j in k:1) {
    if (j == k) {
      if (is.null(ID_labels) & k == 1) {
        sample_by_level[[j]] <-
          sample(1:nrow(data), N[j], replace = TRUE)
      }
      sample_by_level[[j]] <-
        sample(data[, ID_labels[j]], N[j], replace = TRUE)
    } else {
      ## now go through each of the units in the level above it
      sample_current_level <- c()
      for (k in sample_by_level[[j + 1]]) {
        sample_current_level <- c(sample_current_level,
                                  sample(data[data[, ID_labels[j + 1]] == k, ID_labels[j]],
                                         round(N[j]), replace = TRUE))
      }
      sample_by_level[[j]] <- sample_current_level
    }
  }
  data <- data[sample_by_level[[1]], , drop = FALSE]

  ## reset row names so they are unique
  rownames(data) <- 1:nrow(data)

  return(data)
}

#' Resample data, including hierarchical data
#'
#' @param data A data.frame, usually provided by the user.
#' @param N The number of sample N to return. If multiple levels are defined in ID_labels, N should be a vector of length `length(ID_labels)`
#' @param ID_labels A character vector of the variables that indicate the data hierarchy, from highest to lowest (i.e., from cities to citizens).
#'
#' @return A data.frame
#'
#' @examples
#'
#' # Bootstrap a dataset without any hierarchy
#'
#' baseline_survey <- fabricate(N = 5, Y_pre = rnorm(N))
#' bootsrapped_data <- resample_data(baseline_survey, N = 10)
#' bootsrapped_data
#'
#' # Bootstrap a hierarchical dataset
#'
#' my_data <-
#' fabricate(
#'   cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
#'   citizens = level(N = 3, income = round(elevation * rnorm(n = N, mean = 5)))
#' )
#'
#' my_data_2 <- resample_data(my_data, N = c(3, 5), ID_labels = c("cities", "citizens"))
#' my_data_2
#'
#'
#'
#' @export
resample_data <- function(data, N, ID_labels = NULL) {
  # setup
  if (missing(N) & is.null(ID_labels)) {
    N <- nrow(data)
  }
  k <- length(N) ## number of levels

  # checks
  if (!is.null(ID_labels) & (k != length(ID_labels))) {
    stop(
      "If you provide more than one ID_labels to resample data for multilevel data, please provide a vector for N of the same length representing the number to resample at each level."
    )
  }

  # Case 1: Single Level
  if (k == 1) {
    data <- bootstrap_single_level(data = data, N = N)
  } else {
    # Case 2: Multi Level

    data <- bootstrap_single_level(data, ID_label = ID_labels[1], N = N[1])

    for (i in 2:k) {
      group_by_set <- ID_labels[1:(i - 1)]
      group_by_list <- as.list(data[, group_by_set, drop = FALSE])
      new_data_list <- split(data, group_by_list)
      new_data_list <-
        lapply(new_data_list,
               bootstrap_single_level,
               ID_label = ID_labels[i],
               N = N[i])
      data <- do.call(rbind, new_data_list)
    }
  }
  rownames(data) <- NULL
  return(data)
}



bootstrap_single_level <-
  function(data, ID_label = NULL, N) {
    if (is.null(ID_label)) {
      boot_indicies <- sample(1:nrow(data), N, replace = TRUE)
    } else {
      boot_ids <-
        sample(unique(data[, ID_label]), size = N, replace = TRUE)
      boot_indicies <- unlist(lapply(boot_ids, function(i) {
        which(data[, ID_label] == i)
      }))
    }
    new_data <- data[boot_indicies, , drop = FALSE]
    return(new_data)
  }



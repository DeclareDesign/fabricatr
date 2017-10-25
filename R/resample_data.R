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
resample_data = function(data, N, ID_labels=NULL) {
  # User didn't provide an N or an ID label, it's clear they just want a regular bootstrap
  if (missing(N) & is.null(ID_labels)) {
    N <- nrow(data)
    return(bootstrap_single_level(data, nrow(data), ID_label=NULL))
  }

  # Error handling
  if (!is.null(ID_labels) & (length(N) != length(ID_labels))) {
    stop(
      "If you provide more than one ID_labels to resample data for multilevel data, please provide a vector for N of the same length representing the number to resample at each level."
    )
  }

  if (any(!ID_labels %in% names(data))) {
    stop(
      "One or more of the ID labels you provided are not columns in the data frame provided."
    )
  }

  if(length(N) > 10) {
    stop(
      "Multi-level bootstrap with more than 10 levels is not advised."
    )
  }

  # Single level bootstrap with explicit bootstrapping on a particular cluster variable
  if(length(N)==1)
  {
    return(bootstrap_single_level(data,
                                  N[1],
                                  ID_label=ID_labels[1]))
  } else {
    # Do the current bootstrap level
    current_boot_values = unique(data[, ID_labels[1]])
    sampled_boot_values = sample(1:length(current_boot_values), N[1], replace=TRUE)
    app = 0

    # Iterate over each thing chosen at the current level
    results_all = lapply(sampled_boot_values, function(i) {
      new_results = resample_data(
        data[data[, ID_labels[1]] == i, ],
        N=N[2:length(N)],
        ID_labels=ID_labels[2:length(ID_labels)]
      )
    })
    #res = rbindlist(results_all)
    res = do.call(rbind, results_all)
    rownames(res) = NULL
    # Return to preceding level
    return(res)
  }
}

bootstrap_single_level <- function(data, ID_label = NULL, N) {
    if(dim(data)[1] == 0) {
      stop("Data being bootstrapped has no rows.")
    }
    if (is.null(ID_label)) {
      # Simple bootstrap
      boot_indices <- sample(1:nrow(data), N, replace = TRUE)
    } else if(!ID_label %in% colnames(data)) {
      stop("ID label provided is not a column in the data being bootstrapped.")
    }  else {
      # Bootstrapping unique values of ID_label (i.e. cluster selection when data
      # are observations, not clusters
      boot_ids <-
        sample(unique(data[, ID_label]), size = N, replace = TRUE)
      # Need to do the unlist-apply approach to ensure each row
      # is appropriately duplicated. Faster than other ways to map
      # cluster ids to row ids.
      boot_indices <- unlist(lapply(boot_ids, function(i) {
        which(data[, ID_label] == i)
      }))
    }
    # Grab the relevant rows
    new_data <- data[boot_indices, , drop = FALSE]

    return(new_data)
}

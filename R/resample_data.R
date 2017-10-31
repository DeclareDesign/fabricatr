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
#'

resample_data = function(data, N, ID_labels=NULL) {
  # Mask internal outer_level and use_dt arguments from view.
  .resample_data_internal(data, N, ID_labels)
}

.resample_data_internal = function(data, N, ID_labels=NULL, outer_level=1, use_dt = NA) {
  # Handle all the data sanity checks in outer_level so we don't have redundant error
  # checks further down the recursion.
  if(outer_level) {
    # Optional usage of data.table to speed up functionality
    # Short-circuit on the is.na to only attempt the package load if necessary.
    if(is.na(use_dt) && requireNamespace("data.table", quietly=T)) {
      use_dt = 1
    } else {
      use_dt = 0
    }

    # User didn't provide an N or an ID label, it's clear they just want a regular bootstrap
    # of N units by row.
    if (missing(N) & is.null(ID_labels)) {
      return(bootstrap_single_level(data, dim(data)[1], ID_label=NULL))
    }

    # No negative or non-numeric Ns
    # Note: this should be rewritten when we implement the "ALL" option for a level.
    if (any(!is.numeric(N) | N%%1 | N<=0)) {
      stop(
        "All specified Ns must be numeric and at least 1."
      )
    }

    # N doesn't match ID labels
    if (!is.null(ID_labels) & (length(N) != length(ID_labels))) {
      stop(
        "If you provide more than one ID_labels to resample data for multilevel data, please provide a vector for N of the same length representing the number to resample at each level."
      )
    }

    # ID_labels looking for some columns we don't have
    if (any(!ID_labels %in% names(data))) {
      stop(
        "One or more of the ID labels you provided are not columns in the data frame provided."
      )
    }

    # Excessive recursion depth
    if(length(N) > 10) {
      stop(
        "Multi-level bootstrap with more than 10 levels is not advised."
      )
    }
  }

  # Single level bootstrap with explicit bootstrapping on a particular cluster variable
  # this is the inner-most recursion
  if(length(N)==1)
  {
    return(bootstrap_single_level(data,
                                  N[1],
                                  ID_label=ID_labels[1]))
  }

  # OK, if not, we need to recurse

  # Split indices of data frame by the thing we're strapping on
  split_data_on_boot_id = split(seq_len(dim(data)[1]), data[,ID_labels[1]])

  # Do the current bootstrap level
  # sample.int is faster than sample(1:length(.)) or sample(seq.len(length(.))
  sampled_boot_values = sample.int(length(split_data_on_boot_id), N[1], replace=TRUE)

  # Iterate over each thing chosen at the current level
  results_all = lapply(sampled_boot_values, function(i) {
    # Get rowids from current bootstrap index, subset based on that
    # pass through the recursed Ns and labels, and remind the inner
    # layer that it doesn't need to sanity check and we already know
    # if data.table is around.
    # The list subset on the split is faster than unlisting
    .resample_data_internal(
      data[split_data_on_boot_id[i][[1]], ],
      N=N[2:length(N)],
      ID_labels=ID_labels[2:length(ID_labels)],
      outer_level=0,
      use_dt = use_dt
    )
  })

  # We could probably gain slight efficiency by only doing the rbind on the
  # outermost loop.
  if(!use_dt) {
    # With no data.table, we need to rbind and then remove row names.
    # Removing row names is as fast this way as other ways to do the same thing
    res = do.call(rbind, results_all)
    rownames(res) = NULL
  } else {
    # User has data.table, give them a speed benefit for it
    res = data.table::rbindlist(results_all)
    # Strip the things that differentiate data.table from data.frame
    # so we hand back something identical.
    class(res) = "data.frame"
    attr(res, ".internal.selfref") = NULL
  }
  # Return to preceding level
  return(res)
}

bootstrap_single_level <- function(data, ID_label = NULL, N) {
  # dim slightly faster than nrow
  if(dim(data)[1] == 0) {
    stop("Data being bootstrapped has no rows.")
  }

  if (is.null(ID_label)) {
    # Simple bootstrap
    return(data[sample(seq_len(dim(data)[1]), N, replace = TRUE), , drop = F])
  } else if(!ID_label %in% colnames(data)) {
    stop("ID label provided is not a column in the data being bootstrapped.")
  }

  # Split data by cluster ID, storing all row indices associated with that cluster ID
  # nrow passes through transparently to dim, so this is slightly faster
  indices_split = split(seq_len(dim(data)[1]), data[, ID_label])
  # Get cluster IDs (not the actual cluster values, the indices of the clusters)
  # sample.int is slightly faster than sample(1:length(.)) or sample(seq_len(length(.))
  boot_ids = sample.int(length(indices_split), size=N, replace=TRUE)
  # Get all row indices associated with every cluster ID combined
  boot_indices = unlist(indices_split[boot_ids], recursive=F, use.names=F)
  # Only take the indices we want (repeats will be handled properly)
  return(data[boot_indices, , drop=F])
}

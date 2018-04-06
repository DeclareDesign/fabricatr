#' Resample data, including hierarchical data
#'
#' This function allows you to resample any data frame. The default mode
#' performs a single resample of size \code{N} with replacement. Users can
#' also specify more complex resampling strategies to resample hierarchical
#' data.
#'
#' @param data A data.frame, usually provided by the user.
#' @param N The number of sample observations to return. If \code{N} is a single
#' scalar and no labels are provided, \code{N} will specify the number of unit
#' observations to resample. If \code{N} is named, or if the \code{ID_labels}
#' argument is specified (in which case, both \code{N} and \code{ID_labels}
#' should be the same length), then the units resampled will be values of the
#' levels resampled (this is useful for, e.g., cluster resampling). If \code{N}
#' is the constant \code{ALL} for any level, all units of this level will be
#' transparently passed through to the next level of resampling.
#' @param ID_labels A character vector of the variables that indicate the data
#' hierarchy, from highest to lowest (i.e., from cities to citizens).
#' @param unique_labels A boolean, defaulting to FALSE. If TRUE, fabricatr will
#' created an extra data frame column depicting a unique version of the ID_label
#' variable resampled on, called <ID_label>_unique.
#'
#' @return A data.frame
#'
#' @examples
#'
#' # Resample a dataset of size N without any hierarchy
#' baseline_survey <- fabricate(N = 50, Y_pre = rnorm(N))
#' bootstrapped_data <- resample_data(baseline_survey)
#'
#' # Specify a fixed number of observations to return
#' baseline_survey <- fabricate(N = 50, Y_pre = rnorm(N))
#' bootstrapped_data <- resample_data(baseline_survey, N = 100)
#'
#' # Resample by a single level of a hierarchical dataset (e.g. resampling
#' # clusters of observations): N specifies a number of clusters to return
#'
#' clustered_survey <- fabricate(
#'   clusters = add_level(N=25),
#'   cities = add_level(N=round(runif(25, 1, 5)),
#'                      population=runif(n = N, min=50000, max=1000000))
#' )
#'
#' cluster_resample <- resample_data(clustered_survey, N = 5, ID_labels = "clusters")
#'
#' # Alternatively, pass the level to resample as a name:
#' cluster_resample_2 <- resample_data(clustered_survey, N=c(clusters = 5))
#'
#' # Resample a hierarchical dataset on multiple levels
#' my_data <-
#' fabricate(
#'   cities = add_level(N = 20, elevation = runif(n = N, min = 1000, max = 2000)),
#'   citizens = add_level(N = 30, age = runif(n = N, min = 18, max = 85))
#' )
#'
#' # Specify the levels you wish to resample:
#' my_data_2 <- resample_data(my_data, N = c(3, 5),
#'                            ID_labels = c("cities", "citizens"))
#'
#' # To resample every unit at a given level, use the ALL constant
#' # This example will resample 10 citizens at each of the cities:
#'
#' passthrough_resample_data <- resample_data(my_data, N = c(cities=ALL, citizens=10))
#'
#' # To ensure a column with unique labels (for example, to calculate block-level
#' # statistics irrespective of sample choices), use the unique_labels=TRUE
#' # argument -- this will produce new columns with unique labels.
#'
#' unique_resample <- resample_data(my_data, N = c(cities=5), unique_labels = TRUE)
#'
#' @export
#'

resample_data <- function(data, N, ID_labels=NULL, unique_labels=FALSE) {
  # Mask internal outer_level and use_dt arguments from view.
  df <- .resample_data_internal(data = data,
                                N = N,
                                ID_labels = ID_labels,
                                unique_labels = unique_labels)
  rownames(df) <- NULL
  return(df)
}

#' Magic number constant to allow users to specify \code{ALL} for passthrough
#' resampling
#'
#' @keywords internal
#' @export
ALL <- -20171101L

.resample_data_internal <- function(data, N, ID_labels=NULL,
                                    unique_labels=FALSE,
                                    outer_level=1, use_dt = TRUE,
                                    label_prefix = "") {

  # Handle all the data sanity checks in outer_level so we don't have redundant
  # error checks further down the recursion.
  if (outer_level) {
    # Optional usage of data.table to speed up functionality
    # Short-circuit on the is.na to only attempt the package load if necessary.
    use_dt <- use_dt && requireNamespace("data.table", quietly = TRUE)

    # User didn't provide an N or an ID label, it's clear they just want a
    # regular bootstrap of N units by row.
    if (missing(N) & is.null(ID_labels)) {
      return(resample_single_level(data, dim(data)[1], ID_label = NULL))
    }

    # No negative or non-numeric Ns unless they are ALL
    if (any(!is.numeric(N) | N %% 1 | (N <= 0 & N != ALL))) {
      stop(
        "All specified Ns must be numeric and at least 1, or the constant ALL ",
        "to keep all units at a level and pass through."
      )
    }

    # Provided names for ID labels AND for names attributes of N vector
    if (!is.null(ID_labels) & !is.null(names(N))) {
      stop(
        "You may provide names of ID_labels as part of N or as part of the ",
        "argument ID_labels but not both."
      )
    }

    # N doesn't match ID labels
    if (!is.null(ID_labels) & (length(N) != length(ID_labels))) {
      stop(
        "If you provide more than one ID_labels to resample data for ",
        "multilevel data, please provide a vector for N of the same length ",
        "representing the number to resample at each level."
      )
    }

    # Some of the names provided for N are null
    if (!is.null(names(N)) && any(is.na(names(N)) | names(N) == "")) {
      stop(
        "If you provide names of levels to resample through the N argument, ",
        "you must provide a name for every level"
      )
    }

    # Copy names from N to ID_labels
    if (!is.null(names(N))) {
      ID_labels <- names(N)
    }

    # ID_labels looking for some columns we don't have
    if (any(!ID_labels %in% names(data))) {
      stop(
        "One or more of the ID labels you provided are not columns in the ",
        "data frame provided."
      )
    }

    # Excessive recursion depth
    if (length(N) > 10) {
      stop(
        "Multi-level resampling with more than 10 levels is not advised."
      )
    }
  }

  # Single level resampling with explicit resampling on a particular cluster
  # variable -- this is the inner-most recursion
  if (length(N) == 1) {
    return(resample_single_level(
      data = data,
      N = N[1],
      ID_label = ID_labels[1],
      unique_labels = unique_labels,
      label_prefix = label_prefix
    ))
  }

  # OK, if not, we need to recurse

  # Split indices of data frame by the thing we're strapping on
  split_data_on_resample_id <- split(seq_len(dim(data)[1]),
                                     data[[ID_labels[1]]])

  # Do the current resample level
  if (N[1] == ALL) {
    # Take each level once -- seq_len should be marginally faster than
    # 1:length(.)
    sampled_resample_values <- seq_len(length(split_data_on_resample_id))
  } else {
    # sample.int is faster than sample(1:length(.)) or sample(seq.len(length(.))
    sampled_resample_values <- sample.int(length(split_data_on_resample_id),
                                          N[1], replace = TRUE)
  }

  if(unique_labels) {
    vector_to_fold <- unname(unlist(lapply(split_data_on_resample_id,
                                           function(i) {
                                             data[i[[1]][1], ][[ID_labels[1]]]
                                             })))
    new_chunk_labels <- uniquify_vector(vector_to_fold,
                                        sampled_resample_values)
  }

  # Iterate over each thing chosen at the current level
  results_all <- lapply(seq_len(length(sampled_resample_values)), function(i) {
    data_segment <-
      data[split_data_on_resample_id[sampled_resample_values[i]][[1]], ,
           drop = FALSE]

    # Adds unique label for level.
    if(unique_labels) {
      data_segment[[paste0(ID_labels[1], "_unique")]] <- paste0(
        label_prefix,
        new_chunk_labels[i])
    }

    # Get rowids from current resample index, subset based on that
    # pass through the recursed Ns and labels, and remind the inner
    # layer that it doesn't need to sanity check and we already know
    # if data.table is around.
    # The list subset on the split is faster than unlisting
    .resample_data_internal(
      data_segment,
      N = N[2:length(N)],
      ID_labels = ID_labels[2:length(ID_labels)],
      outer_level = 0,
      use_dt = use_dt,
      unique_labels = unique_labels,
      label_prefix = paste0(new_chunk_labels[i], "_")
    )
  })

  # We could probably gain slight efficiency by only doing the rbind on the
  # outermost loop.
  if (!use_dt) {
    # With no data.table, we need to rbind and then remove row names.
    # Removing row names is as fast this way as other ways to do the same thing
    res <- do.call(rbind, results_all)
    rownames(res) <- NULL
  } else {
    # User has data.table, give them a speed benefit for it
    res <- data.table::rbindlist(results_all)
    # Strip the things that differentiate data.table from data.frame
    # so we hand back something identical.
    class(res) <- "data.frame"
    attr(res, ".internal.selfref") <- NULL
  }
  # Return to preceding level
  return(res)
}

resample_single_level <- function(data, ID_label = NULL, N,
                                  unique_labels = FALSE,
                                  label_prefix = "") {
  # dim slightly faster than nrow
  if (dim(data)[1] == 0) {
    stop("Data being resampled has no rows.")
  }

  if (is.null(ID_label)) {
    # Simple bootstrap
    ids <- sample(seq_len(dim(data)[1]), N, replace = TRUE)
    return(data[ids, , drop = FALSE])

  } else if (!ID_label %in% colnames(data)) {
    stop("`ID_label` provided (", ID_label, ") is not a column in the data ",
         "being resampled.")
  }

  if (length(N) > 1) {
    stop("For a single resample level, `N` should be a single positive ",
         "integer. `N` was ", N)
  }

  if (!is.numeric(N) || (N %% 1 | (N <= 0 & N != ALL))) {
    stop("For a single resample level, `N` should be a positive integer. ",
         "`N` was ", N)
  }

  # Split data by cluster ID, storing all row indices associated with that
  # cluster ID. nrow passes through transparently to dim, so this is slightly
  # faster
  indices_split <- split(seq_len(dim(data)[1]), data[[ID_label]])

  # Get cluster IDs (not the actual cluster values, the indices of the
  # clusters)
  if (N == ALL) {
    # User wants passthrough resampling
    # seq_len should be a little faster than 1:length(.)
    resample_ids <- seq_len(length(indices_split))
    warning(
      "You do not need to specify ALL for the final level of your ",
      "resampling plan. By default any excluded levels implicitly keep ",
      "all units at this level."
    )
  } else {
    # sample.int is slightly faster than sample(1:length(.)) or
    # sample(seq_len(length(.))
    resample_ids <- sample.int(length(indices_split), size = N, replace = TRUE)
  }

  if(unique_labels) {
    # Which vector we need to uniquify
    vector_to_fold <- unname(unlist(lapply(indices_split, function(i) {
      data[i[[1]], ][[ID_label]]
    })))

    # What the unique labels are
    new_chunk_labels <- uniquify_vector(vector_to_fold, resample_ids)

    # How many times we need to repeat each
    number_of_expansions <- unname(unlist(lapply(resample_ids, function(i) {
      length(indices_split[i][[1]])
    })))

    # Expand to drop in new column
    expand_new_chunk_labels <- rep(new_chunk_labels,
                                  times = number_of_expansions)
  }


  # Get all row indices associated with every cluster ID combined
  resample_indices <- unlist(
    indices_split[resample_ids],
    recursive = FALSE,
    use.names = FALSE
  )
  # Only take the indices we want (repeats will be handled properly)
  df <- data[resample_indices, , drop = FALSE]

  # Uniquify the label vector if necessary
  if(unique_labels) {
    df[[paste0(ID_label, "_unique")]] <- paste0(
      label_prefix,
      expand_new_chunk_labels
    )
  }

  # Return
  return(df)
}

#' @importFrom stats ave
uniquify_vector <- function(vector, indices) {
  # Force to character to avoid this.
  if(is.factor(vector)) { vector <- as.character(vector) }
  # Generate the unique version.
  as.character(interaction(vector[indices],
                           ave(vector[indices], indices, FUN=seq_along),
                           sep="_"))
}

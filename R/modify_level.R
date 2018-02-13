#' @importFrom rlang quos get_expr
#'
#' @rdname fabricate
#' @export
modify_level <- function(N = NULL, ...) {
  data_arguments <- quos(...)
  if ("working_environment_" %in% names(data_arguments)) {
    working_environment_ <- get_expr(data_arguments[["working_environment_"]])
    data_arguments[["working_environment_"]] <- NULL
  } else {
    # This happens if either an add_level call is run external to a fabricate
    # call OR if add_level is the only argument to a fabricate call and
    # the data argument tries to resolve an add_level call.
    stop("`modify_level()` calls must be run inside `fabricate()` calls.")
  }
  if ("ID_label" %in% names(data_arguments)) {
    ID_label <- get_expr(data_arguments[["ID_label"]])
    data_arguments[["ID_label"]] <- NULL
  }

  return(modify_level_internal(
    N = N, ID_label = ID_label,
    working_environment_ = working_environment_,
    data_arguments = data_arguments
  ))
}

#' @importFrom rlang eval_tidy
#'
modify_level_internal <- function(N = NULL, ID_label = NULL,
                                  working_environment_ = NULL,
                                  data_arguments=NULL) {

  # Need to supply an ID_label, otherwise we have no idea what to modify.
  if (is.null(ID_label)) {
    stop(
      "You can't modify a level without a known level ID variable. If you ",
      "are trying to add nested data, please use `add_level()`"
    )
  }

  # First, establish that if we have no working data frame, we can't continue
  if (is.null(dim(working_environment_$data_frame_output_))) {
    stop(
      "You can't modify a level if there is no working data frame to ",
      "modify: you must either load pre-existing data or generate some data ",
      "before modifying."
    )
  }

  # There are two possibilities. One is that we are modifying the lowest level
  # of data. In which case, we simply add variables, like if someone called
  # add_level with a dataset. To check if that's the world we're in, check if
  # we have any duplicates in the ID label:
  if (!anyDuplicated(working_environment_$data_frame_output_[[ID_label]])) {
    # There is no subsetting going on, but modify_level was used anyway.
    N <- nrow(working_environment_$data_frame_output_)

    # Coerce the working data frame into a list
    working_data_list <- as.list(working_environment_$data_frame_output_)

    # Now loop over the variable creation.
    for (i in names(data_arguments)) {
      # Evaluate the formula in an environment consisting of:
      # 1) The current working data list
      # 2) A list that tells everyone what N means in this context.
      # Store it in the current environment
      working_data_list[[i]] <- eval_tidy(
        data_arguments[[i]],
        append(working_data_list, list(N = N))
      )

      # Write the variable name to the list of variable names
      add_variable_name(working_environment_, i)

      # Nuke the current data argument -- if we have the same variable name
      # created twice, this is OK, because it'll only nuke the current one.
      data_arguments[[i]] <- NULL
    }

    # Before handing back data, ensure it's actually rectangular
    working_data_list <- check_rectangular(working_data_list, N)

    # Overwrite the working data frame.
    working_environment_[["data_frame_output_"]] <- data.frame(
      working_data_list,
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    # Return results
    return(working_environment_)
  }

  # If we're here, then at least some subsetting is used in the modify call
  # first, subset to unique observations, then generate new data, then
  # re-expand. To do this, we need a mapping between observations and unique
  # observations. First, get the unique values of the level:
  unique_values_of_level <- unique(
    working_environment_$data_frame_output_[[ID_label]]
    )

  index_maps <- as.numeric(factor(
    working_environment_$data_frame_output_[[ID_label]],
    levels = unique_values_of_level,
    labels = seq_len(length(unique_values_of_level))
  ))

  # Now, which variables are we going to write to (do we need to subset)?
  write_variables <- unname(unlist(get_symbols_from_quosures(data_arguments)))
  # Remove the ID label from the variables we are going to write to.
  write_variables <- setdiff(write_variables, ID_label)
  # Let's also remove anything that doesn't seem to be a valid variable
  write_variables <- write_variables[write_variables %in%
    names(working_environment_$data_frame_output_)]

  # Level unique variables:
  level_unique_variables <- get_unique_variables_by_level(
    data = working_environment_$data_frame_output_,
    ID_label = ID_label,
    superset = write_variables
  )

  # Error if we try to write using a variable that's not unique to the level.
  if (length(level_unique_variables) != length(write_variables) &
    length(write_variables) != 0) {
    stop(
      "Your modify_level command attempts to generate a new variable at the ",
      "level \"", ID_label,
      "\" but requires reading from the existing variable(s) [",
      paste(setdiff(write_variables, level_unique_variables), collapse = ", "),
      "] which are not defined at the level \"", ID_label, "\"\n\n",
      "To prevent this error, you may modify the data at the level of ",
      "interest, or change the definition of your new variables."
    )
  }

  # Our subset needs these columns -- the level variable, all the unique
  # variables we are going to use to write, and then in case the latter is "",
  # remove that dummy obs.:
  merged_set <- unique(c(ID_label, setdiff(level_unique_variables, "")))

  # And these rows:
  row_indices_keep <- !duplicated(
    working_environment_$data_frame_output_[[ID_label]])

  # Now subset it:
  working_subset <- working_environment_$data_frame_output_[
    row_indices_keep,
    merged_set,
    drop = FALSE
  ]

  # Set the N variable correctly moving forward:
  super_N <- nrow(working_environment_$data_frame_output_)
  N <- nrow(working_subset)

  # Get the subset into a list:
  working_data_list <- as.list(working_subset)
  # And our original working data frame:
  super_working_data_list <- as.list(working_environment_$data_frame_output_)

  # Now loop
  for (i in names(data_arguments)) {
    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    # Store it in the currently working data list
    working_data_list[[i]] <- eval_tidy(
      data_arguments[[i]],
      append(working_data_list, list(N = N))
    )

    # Write the variable name to the list of variable names
    add_variable_name(working_environment_, i)

    # Expand the variable and store it in the actual, expanded working data
    # list. Why do we keep these in parallel? Because subsequent variables
    # might need the non-expanded version to generate new variables.
    super_working_data_list[[i]] <- working_data_list[[i]][index_maps]

    # Nuke the current data argument -- if we have the same variable name
    # created twice, this is OK, because it'll only nuke the current one.
    data_arguments[[i]] <- NULL
  }

  # Before handing back data, ensure it's actually rectangular
  super_working_data_list <- check_rectangular(super_working_data_list,
                                               super_N)

  # Overwrite the working data frame.
  working_environment_$data_frame_output_ <- data.frame(
    super_working_data_list,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Return results
  return(working_environment_)
}

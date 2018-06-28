#' @importFrom rlang quos get_expr
#'
#' @rdname fabricate
#' @export
modify_level <- function(N = NULL, ...) {
  data_arguments <- quos(...)

  if(!has_name(data_arguments, "working_environment_")){
    # This happens if either an add_level call is run outside of fabricate()
    stop("`modify_level()` calls must be run inside `fabricate()` calls.")
  }

  working_environment_ <- get_expr(data_arguments[["working_environment_"]])
  data_arguments[["working_environment_"]] <- NULL

  if (has_name(data_arguments, "ID_label")) {
    ID_label <- get_expr(data_arguments[["ID_label"]])
    data_arguments[["ID_label"]] <- NULL
  }

  modify_level_internal(
    N = N, ID_label = ID_label,
    working_environment_ = working_environment_,
    data_arguments = data_arguments
  )
}

#' @importFrom rlang eval_tidy
#'
modify_level_internal <- function(N = NULL, ID_label = NULL,
                                  working_environment_ = NULL,
                                  data_arguments=NULL) {


  modify_level_internal_checks(ID_label, working_environment_)

  workspace <- working_environment_
  uu <- attr(workspace, "active_df")

  df <- workspace[[uu]]



  # There are two possibilities. One is that we are modifying the lowest level
  # of data. In which case, we simply add variables, like if someone called
  # add_level with a dataset. To check if that's the world we're in, check if
  # we have any duplicates in the ID label:
  if (!anyDuplicated(df[[ID_label]])) {
    # There is no subsetting going on, but modify_level was used anyway.
    N <- nrow(df)

    # Coerce the working data frame into a list
    working_data_list <- as.list(df)


    # Now loop over the variable creation.
    for (i in names(data_arguments)) {
      # Evaluate the formula in an environment consisting of:
      # 1) The current working data list
      # 2) A list that tells everyone what N means in this context.
      # Store it in the current environment
      working_data_list[[i]] <- expand_or_error(eval_tidy(
        data_arguments[[i]],
        append(working_data_list, list(N = N))
      ), N, i, data_arguments[[i]])

      # Write the variable name to the list of variable names
      add_variable_name(working_environment_, i)

      # Nuke the current data argument -- if we have the same variable name
      # created twice, this is OK, because it'll only erase the first one.
      data_arguments[[i]] <- NULL
    }

    # Before handing back data, ensure it's actually rectangular
    working_data_list <- check_rectangular(working_data_list, N)

    # Overwrite the working data frame.
    workspace[[uu]] <- data.frame(
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
  unique_values_of_level <- unique(df[[ID_label]])

  index_maps <- match(df[[ID_label]], unique_values_of_level)

  # Now, which variables are we going to from to (do we need to subset)?
  input_variables <- unname(unlist(get_symbols_from_quosures(data_arguments)))
  input_variables <- intersect(setdiff(input_variables, ID_label), names(df))
  input_variables <- df[input_variables]

  # Level unique variables:
  level_unique_variables <- get_unique_variables_by_level(
    data = df,
    ID_label = ID_label,
    superset = input_variables
  )

  check_uniqueness_at_level(level_unique_variables, input_variables, ID_label)


  # Our subset needs these columns -- the level variable, all the unique
  # variables we are going to use to write, and then in case the latter is "",
  # remove that dummy obs.:
  merged_set <- unique(c(ID_label, setdiff(level_unique_variables, "")))

  # And these rows:
  row_indices_keep <- !duplicated(
    df[[ID_label]])

  # Now subset it:
  working_subset <- df[
    row_indices_keep,
    merged_set,
    drop = FALSE
  ]

  # Set the N variable correctly moving forward:
  super_N <- nrow(df)
  N <- nrow(working_subset)

  # Get the subset into a list:
  working_data_list <- as.list(working_subset)
  # And our original working data frame:
  super_working_data_list <- as.list(df)

  # Now loop
  for (i in names(data_arguments)) {
    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    # Store it in the currently working data list
    working_data_list[[i]] <- expand_or_error(eval_tidy(
      data_arguments[[i]],
      append(working_data_list, list(N = N))
    ), N, i, data_arguments[[i]])

    # Write the variable name to the list of variable names
    add_variable_name(working_environment_, i)

    # Expand the variable and store it in the actual, expanded working data
    # list. Why do we keep these in parallel? Because subsequent variables
    # might need the non-expanded version to generate new variables.
    super_working_data_list[[i]] <- working_data_list[[i]][index_maps]

    # clean up as above
    data_arguments[[i]] <- NULL
  }

  # Before handing back data, ensure it's actually rectangular
  super_working_data_list <- check_rectangular(super_working_data_list, super_N)

  # Overwrite the working data frame.
  working_environment_[[uu]] <- data.frame(
    super_working_data_list,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Return results
  working_environment_
}




modify_level_internal_checks <- function(ID_label, workspace) {
  # Need to supply an ID_label, otherwise we have no idea what to modify.
  if (is.null(ID_label)) {
    stop(
      "You can't modify a level without a known level ID variable. If you ",
      "are trying to add nested data, please use `add_level()`"
    )
  }

  uu <- attr(workspace, "active_df")

  # First, establish that if we have no working data frame, we can't continue
  if (is.null(dim(workspace[[uu]]))) {
    stop(
      "You can't modify a level if there is no working data frame to ",
      "modify: you must either load pre-existing data or generate some data ",
      "before modifying."
    )
  }
}




check_uniqueness_at_level <- function(level_unique_variables, write_variables, ID_label) {
  # Error if we try to write using a variable that's not unique to the level.
  if (length(level_unique_variables) != length(write_variables) &&
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
}

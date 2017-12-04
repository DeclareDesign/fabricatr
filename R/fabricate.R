#' Fabricate data
#'
#' \code{fabricate} helps you simulate a dataset before you collect it. You can
#' either start with your own data and add simulated variables to it (by passing
#' \code{data} to \code{fabricate()}) or start from scratch by defining
#' \code{N}. Create hierarchical data with multiple levels of data such as
#' citizens within cities within states using \code{add_level()} or modify
#' existing hierarchical data using \code{modify_level()}. You can use any R
#' function to create each variable. We provide several built-in options to
#' easily draw from binary and count outcomes, \code{\link{draw_binary}},
#' \code{\link{draw_binary_icc}}, and \code{\link{draw_discrete}}.
#'
#' @param data (optional) user-provided data that forms the basis of the
#' fabrication, i.e. you can add variables to existing data. Provide either
#' \code{N} or \code{data} (\code{N} is the number of rows of the data if
#' \code{data} is provided).
#' @param N (optional) number of units to draw. If provided as
#' \code{fabricate(N = 5)}, this determines the number of units in the
#' single-level data. If provided in \code{add_level}, i.e.
#' \code{fabricate(cities = add_level(N = 5))}, \code{N} determines the number
#' of units in a specific level of a hierarchical dataset.
#' @param ID_label (optional) variable name for ID variable, i.e. citizen_ID
#' @param ... Variable or level-generating arguments, such as
#' \code{my_var = rnorm(N)}. For \code{fabricate}, you may also pass
#' \code{add_level()} or \code{modify_level()} arguments, which define a level
#' of a multi-level dataset. See examples.
#' @param new_hierarchy Reserved argument for future functionality to add
#' cross-classified data. Not yet implemented.
#' @param working_environment_ Internal argument, not intended for end-user use.
#' @param data_arguments Internal argument, not intended for end-user use.
#'
#' @return data.frame
#'
#' @examples
#'
#' # Draw a single-level dataset with no covariates
#' df <- fabricate(N = 100)
#' head(df)
#'
#' # Draw a single-level dataset with a covariate
#' df <- fabricate(
#'   N = 100,
#'   height_ft = runif(N, 3.5, 8)
#' )
#' head(df)
#'
#' # Start with existing data
#' df <- fabricate(
#'   data = df,
#'   new_variable = rnorm(N)
#' )
#'
#' # Draw a two-level hierarchical dataset
#' # containing cities within regions
#' df <- fabricate(
#'  regions = add_level(N = 5),
#'  cities = add_level(N = 2, pollution = rnorm(N, mean = 5)))
#' head(df)
#'
#' # Start with existing data and add variables to hierarchical data
#' # at levels which are already present in the existing data.
#' # Note: do not provide N when adding variables to an existing level
#' df <- fabricate(
#'   data = df,
#'   regions = modify_level(watershed = sample(c(0, 1), N, replace = TRUE)),
#'   cities = modify_level(runoff = rnorm(N))
#' )
#'
#' @importFrom rlang quos quo_name eval_tidy lang_name lang_modify lang_args
#' is_lang get_expr
#'
#' @export
fabricate <- function(data = NULL, ..., N = NULL, ID_label = NULL)
{
  # Store all data generation arguments in a quosure for future evaluation
  # A quosure contains unevaluated formulae and function calls.
  data_arguments = quos(...)

  # Fabricatr expects either a single-level function call
  # or a series of level calls. You can't mix and match.
  # This helper function will be TRUE if calls are all levels, FALSE
  # if there are no calls or they are not levels.
  all_levels = check_all_levels(data_arguments)

  # User must provide exactly one of:
  # 1) One or more level calls (with or without importing their own data)
  # 2) Import their own data and do not involve level calls
  # 3) Provide an N without importing their own data
  if(sum((!is.null(data) & !missing(data) & !all_levels),
         (!is.null(N) & !missing(N)),
         all_levels) != 1) {
    stop(
      "Fabricate can be called in one of three ways: \n",
      "1) Provide one or more level calls, with or without existing data \n",
      "2) Provide existing data and add new variables without adding a level \n",
      "3) Provide an \"N\" and add new variables"
      )
  }

  # Create a blank working environment.
  working_environment = new.env()

  # User provided level calls
  if(all_levels) {

    # Ensure the user provided a name for each level call.
    if(is.null(names(data_arguments)) | any(names(data_arguments) == "")) {
      stop("You must provide a name for each level you create.")
    }

    # User provided data, if any, should be preloaded into working environment
    if(!is.null(data) & !missing(data)) {
      # Ensure data is sane.
      data = handle_data(data)
      working_environment$imported_data_ = data
    }

    # Each of data_arguments is a level call
    for(i in seq_along(data_arguments)) {
      # Add two variables to the argument of the current level call
      # one to pass the working environment so far
      # one to pass the ID_label the user intends for the level

      data_arguments[[i]] = lang_modify(data_arguments[[i]],
                                        working_environment_ = working_environment,
                                        ID_label = names(data_arguments)[i])

      # Execute the level build and pass it back to the current working
      # environment.
      working_environment = eval_tidy(data_arguments[[i]])
    }

    # Return the results from the working environment
    return(report_results(working_environment))
  }

  # User did not pass data -- they passed N
  if(is.null(data) | missing(data)) {
    # Single level -- maybe the user provided an ID_label, maybe they didn't.
    # Sanity check and/or construct an ID label for the new data.
    ID_label = handle_id(ID_label, data)

    # Is the N argument passed here sane? Let's check
    N = handle_n(N, add_level = TRUE)

    # Creating a working environment that's empty (user passed no data)
    data_arguments[["working_environment_"]] = working_environment

    # Run the level adder, report the results, and return
    return(
      report_results(
        add_level(N = N, ID_label = ID_label, data_arguments = data_arguments)
      )
    )
  }

  # Confirm data can be a data frame
  data = handle_data(data)

  # Single level -- maybe the user provided an ID_label, maybe they didn't.
  # Sanity check and/or construct an ID label for the new data.
  ID_label = handle_id(ID_label, data)

  # User passed data, not N
  # First, let's dynamically get N from the number of rows
  N = nrow(data)

  # Now, let's load the data into our working environment
  working_environment$imported_data_ = data
  data_arguments[["working_environment_"]] = working_environment

  # Run the level adder, report the results, and return
  return(
    report_results(
      add_level(N = N,
                ID_label = ID_label,
                data_arguments = data_arguments,
                new_hierarchy=TRUE)
    )
  )
}

#' @importFrom rlang quos eval_tidy quo lang_modify
#'
#' @rdname fabricate
#' @export
add_level = function(N = NULL, ID_label = NULL,
                     working_environment_ = NULL,
                     ...,
                     data_arguments=quos(...),
                     new_hierarchy = FALSE) {

  # Copy the working environment out of the data_arguments quosure and into
  # the root. This happens when we have a single non-nested fabricate call
  # and we don't want to double-quosure the working environmented.
  if("working_environment_" %in% names(data_arguments)) {
    working_environment_ = data_arguments[["working_environment_"]]
    data_arguments[["working_environment_"]] = NULL
  }

  # Pass-through mapper to nest_level.
  # This needs to be done after we read the working environment and
  # before we check N or do the shelving procedure.
  if(!new_hierarchy &
     ("data_frame_output_" %in% names(working_environment_) |
     "imported_data_" %in% names(working_environment_))) {
    return(nest_level(N=N, ID_label=ID_label,
                      working_environment_=working_environment_,
                      data_arguments=data_arguments))
  }

  # Check to make sure the N here is sane
  N = handle_n(N, add_level=TRUE)

  # User is adding a new level, but already has a working data frame.
  # Shelf the working data frame and move on
  if("data_frame_output_" %in% names(working_environment_)) {
    # Construct the shelved version
    package_df = list(data_frame_output_ = working_environment_$data_frame_output_,
                      level_ids_ = working_environment_$level_ids_,
                      variable_names_ = names(working_environment_$data_frame_output_))

    # Append it to the existing shelf
    if("shelved_df" %in% names(working_environment_)) {
      working_environment_$shelved_df = append(working_environment_$shelved_df, package_df)
    } else {
      # Create a shelf just for this
      working_environment_$shelved_df = list(package_df)
    }

    # Clear the current work-space.
    working_environment_$data_frame_output_ =
      working_environment_$level_ids_ =
      working_environment_$variable_names_ = NULL
  }

  # User is adding a new level, but we need to sneak in the imported data first.
  # When this is done, trash the imported data, because the working data frame
  # contains it.
  if("imported_data_" %in% names(working_environment_)) {
    num_obs_imported = nrow(working_environment_$imported_data_)
    working_data_list = as.list(working_environment_$imported_data_)
    working_environment_$variable_names_ = names(working_environment_$imported_data_)
    working_environment_$imported_data_ = NULL

    # User didn't specify an N, so get it from the current data.
    if(is.null(N)) {
      N = num_obs_imported
    }
  } else {
    working_data_list = list()
  }

  # Staple in an ID column onto the data list.
  if(!is.null(ID_label)) {
    # It's possible the working data frame already has the ID label, if so,
    # don't do anything.
    if(is.null(names(working_data_list)) || !ID_label %in% names(working_data_list)) {
      # First, add the column to the working data frame
      working_data_list[[ID_label]] = generate_id_pad(N)

      # Next, add the ID_label to the level ids tracker
      # Why does this not need to return? Because environments are passed by
      # reference
      add_level_id(working_environment_, ID_label)
      add_variable_name(working_environment_, ID_label)
    } else {
      # If the ID label was specified but already exists, we should still log
      # it as a level ID
      add_level_id(working_environment_, ID_label)
    }
  } else {
    stop("Please specify a name for the level call you are creating.")
  }

  # Loop through each of the variable generating arguments
  for(i in names(data_arguments)) {
    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    working_data_list[[i]] = eval_tidy(data_arguments[[i]],
                                       append(working_data_list, list(N=N)))

    # Write the variable name to the list of variable names
    add_variable_name(working_environment_, i)

    # Nuke the current data argument -- if we have the same variable name
    # created twice, this is OK, because it'll only nuke the current one.
    data_arguments[[i]] = NULL
  }

  # Before handing back data, ensure it's actually rectangular
  working_data_list = check_rectangular(working_data_list, N)

  # Coerce our working data list into a working data frame
  working_environment_$data_frame_output_ = data.frame(working_data_list,
                                                       stringsAsFactors=FALSE,
                                                       row.names=NULL)

  # In general the reference should be unchanged, but for single-level calls
  # there won't be a working environment to reference.
  return(working_environment_)
}

#'
#' @importFrom rlang quos eval_tidy quo lang_modify
#'
nest_level = function(N = NULL, ID_label = NULL,
                      working_environment_ = NULL,
                      ...,
                      data_arguments=quos(...)) {

  # Check to make sure we have a data frame to nest on.
  if(is.null(dim(working_environment_$data_frame_output_))) {
    if("imported_data_" %in% names(working_environment_)) {
      working_environment_$data_frame_output_ = data.frame(working_environment_$imported_data_)
      working_environment_$variable_names_ = names(working_environment_$imported_data_)
      working_environment_$imported_data_ = NULL
    } else {
      stop("You can't nest a level if there is no level to nest inside")
    }
  }

  # Check to make sure the N here is sane
  # Pass the working environment because N might not be a singleton here
  N = handle_n(N, add_level=FALSE,
               working_environment = working_environment_,
               parent_frame_levels=2)

  # We need to expand the size of the current working data frame by copying it
  # Let's start by getting the size of the current working data frame
  past_level_N = nrow(working_environment_$data_frame_output_)
  # And now make an index set 1:past_level_N
  indices = seq_len(past_level_N)

  # We're now going to modify the index set to take into account the expansion
  # If N is a single number, then we repeat each index N times
  # If N is of length past_level_N, then we repeat each index N_i times.
  # For r's rep, the each / times arguments have odd behaviour that necessitates
  # this approach
  if(length(N)==1) rep_indices = rep(indices, each=N)
  else rep_indices = rep(indices, times=N)

  # Update N to the new length.
  inner_N = N # Length specified for this level
  N = length(rep_indices) # Length of overall data frame

  # Expand the data frame by duplicating the indices and then coerce the data
  # frame to a list -- we do this to basically make variables accessible in the
  # namespace.
  working_data_list = as.list(working_environment_$data_frame_output_[rep_indices, , drop=FALSE])

  # Everything after here is non-unique to nest_level versus add_level -- need
  # to think about how to refactor this out.

  # Staple in an ID column onto the data list.
  if(!is.null(ID_label) && (is.null(names(working_data_list)) ||
                            !ID_label %in% names(working_data_list))) {
    # First, add the column to the working data frame
    working_data_list[[ID_label]] = generate_id_pad(N)

    add_level_id(working_environment_, ID_label)
    add_variable_name(working_environment_, ID_label)
  }

  # Loop through each of the variable generating arguments
  for(i in names(data_arguments)) {
    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    working_data_list[[i]] = eval_tidy(data_arguments[[i]],
                                       append(working_data_list, list(N=N)))

    # User provided a fixed-length data variable whose length is the length of
    # the inner-most level for a given outer level. See example:
    # fabricate(countries = add_level(N=20),
    #           cities = nest_level(N=2, capital=c(TRUE, FALSE)))
    # We need to expand this to each setting of the outer level.
    # Only evaluate if inner_N is a single number
    if(length(inner_N) == 1 && length(working_data_list[[i]]) == inner_N) {
      working_data_list[[i]] = rep(working_data_list[[i]], (N/inner_N))
    }

    # Write the variable name to the list of variable names
    add_variable_name(working_environment_, i)

    # Nuke the current data argument -- if we have the same variable name
    # created twice, this is OK, because it'll only nuke the current one.
    data_arguments[[i]] = NULL
  }

  # Before handing back data, ensure it's actually rectangular
  working_data_list = check_rectangular(working_data_list, N)

  # Overwrite the working data frame.
  working_environment_[["data_frame_output_"]] = data.frame(working_data_list,
                                                            stringsAsFactors=FALSE,
                                                            row.names=NULL)

  return(working_environment_)
}

#' @importFrom rlang quos eval_tidy quo lang_modify
#'
#' @rdname fabricate
#' @export
modify_level = function(N = NULL,
                            ID_label = NULL,
                            working_environment_ = NULL,
                            ...,
                            data_arguments=quos(...)) {

  # Need to supply an ID_label, otherwise we have no idea what to modify.
  # You actually can, though! It'd just be per unit
  if(is.null(ID_label)) {
    stop("You can't modify a level without a known level ID variable. If you",
         "are adding nested data, please use add_level")
  }

  # First, establish that if we have no working data frame, we can't continue
  if(is.null(dim(working_environment_$data_frame_output_))) {
    if("imported_data_" %in% names(working_environment_)) {
      working_environment_$data_frame_output_ = data.frame(working_environment_$imported_data_)
      working_environment_$variable_names_ = names(working_environment_$imported_data_)
      working_environment_$imported_data_ = NULL
    } else {
      stop(
        "You can't modify a level if there is no working data frame to ",
        "modify: you must either load pre-existing data or generate some data ",
        "before modifying."
        )
    }
  }

  # There are two possibilities. One is that we are modifying the lowest level
  # of data. In which case, we simply add variables, like if someone called
  # add_level with a dataset. To check if that's the world we're in, check if
  # we have any duplicates in the ID label:
  if(!anyDuplicated(working_environment_$data_frame_output_[[ID_label]])) {
    # There is no subsetting going on, but modify_level was used anyway.
    N = nrow(working_environment_$data_frame_output_)

    # Coerce the working data frame into a list
    working_data_list = as.list(working_environment_$data_frame_output_)

    # Now loop over the variable creation.
    for(i in names(data_arguments)) {
      # Evaluate the formula in an environment consisting of:
      # 1) The current working data list
      # 2) A list that tells everyone what N means in this context.
      # Store it in the current environment
      working_data_list[[i]] = eval_tidy(data_arguments[[i]],
                                         append(working_data_list, list(N=N)))

      # Write the variable name to the list of variable names
      add_variable_name(working_environment_, i)

      # Nuke the current data argument -- if we have the same variable name
      # created twice, this is OK, because it'll only nuke the current one.
      data_arguments[[i]] = NULL
    }

    # Before handing back data, ensure it's actually rectangular
    working_data_list = check_rectangular(working_data_list, N)

    # Overwrite the working data frame.
    working_environment_[["data_frame_output_"]] = data.frame(working_data_list,
                                                              stringsAsFactors=FALSE,
                                                              row.names=NULL)

    # Return results
    return(working_environment_)
  }

  # If we're here, then at least some subsetting is used in the modify call
  # first, subset to unique observations, then generate new data, then re-expand.
  # To do this, we need a mapping between observations and unique observations.
  # First, get the unique values of the level:
  unique_values_of_level = unique(working_environment_$data_frame_output_[[ID_label]])

  # Pre-allocate the mapping vector
  index_maps = numeric(length(working_environment_$data_frame_output_[[ID_label]]))
  # Iterate along the unique values of the level
  for(i in seq_along(unique_values_of_level)) {
    # Any obs that matches the level matching this i will be a duplicate of this i.
    index_maps[
      working_environment_$data_frame_output_[[ID_label]] == unique_values_of_level[i]
      ] = i
  }

  # Now, which variables are we going to write to (do we need to subset)?
  write_variables = unname(unlist(get_symbols_from_quosure(data_arguments)))
  # Remove the ID label from the variables we are going to write to.
  write_variables = setdiff(write_variables, ID_label)
  # Let's also remove anything that doesn't seem to be a valid variable
  write_variables = write_variables[write_variables %in% names(working_environment_$data_frame_output_)]

  # Level unique variables:
  level_unique_variables = get_unique_variables_by_level(
    data = working_environment_$data_frame_output_,
    ID_label = ID_label,
    superset=write_variables)

  # Error if we try to write using a variable that's not unique to the level.
  if(length(level_unique_variables) != length(write_variables) &
     length(write_variables) != 0) {
    stop(
      "Your modify_level command attempts to generate a new variable at the level \"",
      ID_label,
      "\" but requires reading from the existing variable(s) [",
      paste(setdiff(write_variables, level_unique_variables), collapse=", "),
      "] which are not defined at the level \"", ID_label, "\"\n\n",
      "To prevent this error, you may modify the data at the level of interest, ",
      "or change the definition of your new variables."
    )
  }

  # Our subset needs these columns -- the level variable, all the unique
  # variables we are going to use to write, and then in case the latter is "",
  # remove that dummy obs.:
  merged_set = unique(c(ID_label, setdiff(level_unique_variables, "")))

  # And these rows:
  row_indices_keep = !duplicated(working_environment_$data_frame_output_[[ID_label]])

  # Now subset it:
  working_subset = working_environment_$data_frame_output_[row_indices_keep,
                                                           merged_set,
                                                           drop=FALSE]

  # Set the N variable correctly moving forward:
  super_N = nrow(working_environment_$data_frame_output_)
  N = nrow(working_subset)

  # Get the subset into a list:
  working_data_list = as.list(working_subset)
  # And our original working data frame:
  super_working_data_list = as.list(working_environment_$data_frame_output_)

  # Now loop
  for(i in names(data_arguments)) {
    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    # Store it in the currently working data list
    working_data_list[[i]] = eval_tidy(data_arguments[[i]],
                                       append(working_data_list, list(N=N)))

    # Write the variable name to the list of variable names
    add_variable_name(working_environment_, i)

    # Expand the variable and store it in the actual, expanded working data list
    # Why do we keep these in parallel? Because subsequent variables might need
    # the non-expanded version to generate new variables.
    super_working_data_list[[i]] = working_data_list[[i]][index_maps]

    # Nuke the current data argument -- if we have the same variable name
    # created twice, this is OK, because it'll only nuke the current one.
    data_arguments[[i]] = NULL
  }

  # Before handing back data, ensure it's actually rectangular
  super_working_data_list = check_rectangular(super_working_data_list, super_N)

  # Overwrite the working data frame.
  working_environment_$data_frame_output_ = data.frame(super_working_data_list,
                                                            stringsAsFactors=FALSE,
                                                            row.names=NULL)

  # Return results
  return(working_environment_)
}

#' Deprecated level call function maintained to provide useful error for
#' previous fabricatr code.
#' @keywords internal
#' @export
level = function(N = NULL, ID_label = NULL, ...) {
  stop("Level calls are currently deprecated; use add_level and modify_level")
  # Stub, this doesn't do anything yet -- may in the future dispatch to the
  # relevant levels.
}

# Dummy helper function that just extracts the working data frame from the
# environment. This exists because we may in the future want to return something
# that is not a data frame.
report_results = function(working_environment) {
  return(working_environment$data_frame_output_)
}

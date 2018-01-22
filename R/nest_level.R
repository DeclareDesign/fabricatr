#' @importFrom rlang quos get_expr quo_text
#'
#' @rdname fabricate
#' @export
nest_level = function(N = NULL,
                     ...) {

  data_arguments = quos(...)
  if("working_environment_" %in% names(data_arguments)) {
    working_environment_ = get_expr(data_arguments[["working_environment_"]])
    data_arguments[["working_environment_"]] = NULL
  }
  if("ID_label" %in% names(data_arguments)) {
    ID_label = get_expr(data_arguments[["ID_label"]])
    data_arguments[["ID_label"]] = NULL
  }

  return(nest_level_internal(N = N, ID_label = ID_label,
                             working_environment_ = working_environment_,
                             data_arguments = data_arguments))
}

#' @importFrom rlang eval_tidy
#'
nest_level_internal = function(N = NULL, ID_label = NULL,
                               working_environment_ = NULL,
                               data_arguments = NULL) {

  # Check to make sure we have a data frame to nest on.
  if(is.null(dim(working_environment_$data_frame_output_))) {
    stop("You can't nest a level if there is no level to nest inside")
  }

  # Check to make sure the N here is sane
  # Pass the working environment because N might not be a singleton here
  N = handle_n(N, add_level=FALSE,
               working_environment = working_environment_,
               parent_frame_levels=3)

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

    if(length(working_data_list[[i]]) != N) {
      warning(
        "Nested data length for the variable \"", i, "\" ",
        "appears to be incorrect. Nested data must either inherit the length N ",
        "or be fixed-length variables equal to the total number of observations ",
        "at the outer level. (In this case, ", N, ")\n\n"
      )
    }

    # Write the variable name to the list of variable names
    add_variable_name(working_environment_, i)

    # Nuke the current data argument -- if we have the same variable name
    # created twice, this is OK, because it'll only nuke the current one.
    data_arguments[[i]] = NULL
  }

  # Before handing back data, ensure it's actually rectangular -- although
  # this should be covered by the error message above.
  working_data_list = check_rectangular(working_data_list, N)

  # Overwrite the working data frame.
  working_environment_[["data_frame_output_"]] = data.frame(working_data_list,
                                                            stringsAsFactors=FALSE,
                                                            row.names=NULL)

  return(working_environment_)
}

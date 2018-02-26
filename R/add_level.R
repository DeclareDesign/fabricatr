#' @importFrom rlang quos get_expr quo_text enquo
#'
#' @rdname fabricate
#' @export
add_level <- function(N = NULL,
                      ...,
                      nest = TRUE) {
  N <- enquo(N)
  data_arguments <- quos(...)
  if ("working_environment_" %in% names(data_arguments)) {
    working_environment_ <- get_expr(data_arguments[["working_environment_"]])
    data_arguments[["working_environment_"]] <- NULL
  } else {
    # This happens if either an add_level call is run external to a fabricate
    # call OR if add_level is the only argument to a fabricate call and
    # the data argument tries to resolve an add_level call.
    stop("`add_level()` calls must be run inside `fabricate()` calls.")
  }

  if ("ID_label" %in% names(data_arguments)) {
    ID_label <- get_expr(data_arguments[["ID_label"]])
    data_arguments[["ID_label"]] <- NULL
  }

  return(add_level_internal(
    N = N, ID_label = ID_label,
    working_environment_ = working_environment_,
    data_arguments = data_arguments,
    nest = nest
  ))
}

#' @importFrom rlang eval_tidy
add_level_internal <- function(N = NULL, ID_label = NULL,
                               working_environment_ = NULL,
                               data_arguments = NULL,
                               nest = TRUE) {

  # Pass-through mapper to nest_level.
  # This needs to be done after we read the working environment and
  # before we check N or do the shelving procedure.
  if (nest && "data_frame_output_" %in% names(working_environment_)) {
    return(nest_level_internal(
      N = N, ID_label = ID_label,
      working_environment_ = working_environment_,
      data_arguments = data_arguments
    ))
  }

  # Check to make sure the N here is sane
  N <- handle_n(N, add_level = TRUE)

  # If the user already has a working data frame, we need to shelf it before
  # we move on.
  working_environment_ <- shelf_working_data(working_environment_)

  if ("data_frame_output_" %in% names(working_environment_)) {
    working_data_list <- as.list(working_environment_$data_frame_output_)
  } else {
    working_data_list <- list()
  }

  # Staple in an ID column onto the data list.
  if (!is.null(ID_label) && !is.na(ID_label)) {
    # It's actually not possible the working data frame already has an ID label
    # since we forcibly shelved it earlier -- so let's just plough along.

    # First, add the column to the working data frame
    working_data_list[[ID_label]] <- generate_id_pad(N)

    # Next, add the ID_label to the level ids tracker
    # Why does this not need to return? Because environments are passed by
    # reference
    add_level_id(working_environment_, ID_label)
    add_variable_name(working_environment_, ID_label)
  } else {
    stop("Please specify a name for the level call you are creating.")
  }

  # Loop through each of the variable generating arguments
  for (i in names(data_arguments)) {
    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    working_data_list[[i]] <- expand_or_error(eval_tidy(
      data_arguments[[i]],
      append(working_data_list, list(N = N))
    ), N, i, data_arguments[[i]])

    # Write the variable name to the list of variable names
    add_variable_name(working_environment_, i)

    # Nuke the current data argument -- if we have the same variable name
    # created twice, this is OK, because it'll only nuke the current one.
    data_arguments[[i]] <- NULL
  }

  # Before handing back data, ensure it's actually rectangular
  working_data_list <- check_rectangular(working_data_list, N)

  # Coerce our working data list into a working data frame
  working_environment_$data_frame_output_ <- data.frame(
    working_data_list,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # In general the reference should be unchanged, but for single-level calls
  # there won't be a working environment to reference.
  return(working_environment_)
}

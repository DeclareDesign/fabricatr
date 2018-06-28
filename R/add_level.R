#' @importFrom rlang quos get_expr quo_text enquo
#'
#' @rdname fabricate
#' @export
add_level <- function(N = NULL, ..., nest = TRUE) {
  do_internal(enquo(N), ..., FUN=add_level_internal, nest=nest)
}

#' @importFrom rlang eval_tidy
add_level_internal <- function(N = NULL, ID_label = NULL,
                               working_environment_ = NULL,
                               data_arguments = NULL,
                               nest = TRUE) {

  # Pass-through mapper to nest_level.
  # This needs to be done after we read the working environment and
  # before we check N or do the shelving procedure.
  if (nest && ("data_frame_output_" %in% names(working_environment_) ||
               is.character(attr(working_environment_, "active_df")))
               ) {
    return(nest_level_internal(
      N = N, ID_label = ID_label,
      working_environment_ = working_environment_,
      data_arguments = data_arguments
    ))
  }

  check_add_level_args(data_arguments, ID_label)


  # Check to make sure the N here is sane
  N <- handle_n(N, add_level = TRUE, working_environment_)

  # If the user already has a working data frame, we need to shelf it before
  # we move on.

  if ("data_frame_output_" %in% names(working_environment_)) {
    working_data_list <- as.list(working_environment_$data_frame_output_)
  } else {
    working_data_list <- list()
  }



  # Staple in an ID column onto the data list.

  # It's actually not possible the working data frame already has an ID label
  # since we forcibly shelved it earlier -- so let's just plough along.

  # First, add the column to the working data frame
  working_data_list[[ID_label]] <- generate_id_pad(N)

  # Next, add the ID_label to the level ids tracker
  # Why does this not need to return? Because environments are passed by
  # reference
  add_level_id(working_environment_, ID_label)
  add_variable_name(working_environment_, ID_label)

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

  working_environment_[[ID_label]] <- working_environment_$data_frame_output_
  attr(working_environment_, "active_df") <- ID_label

  rm("data_frame_output_", envir = working_environment_)

  # In general the reference should be unchanged, but for single-level calls
  # there won't be a working environment to reference.
  working_environment_
}


check_add_level_args <- function(data_arguments, ID_label) {
  if(any(names(data_arguments) == "")) {
    stop("All variables inside an add_level call must be named.")
  }

  if(is.null(ID_label) || is.na(ID_label)) {
    stop("Please specify a name for the level call you are creating.")

  }
}


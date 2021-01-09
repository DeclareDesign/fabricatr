#' @importFrom rlang quos get_expr quo_text enquo
#'
#' @rdname fabricate
#' @export
add_level <- function(N = NULL, ..., nest = TRUE) {
  fun <- if(nest && can_nest(...)) nest_level_internal else add_top_level_internal
  do_internal(enquo(N), ..., FUN=fun, from="add_level")
}


can_nest <- function(...){
  pred <- function(N, ID_label, workspace, data_arguments)
    is.character(attr(workspace, "active_df"))
  do_internal(N=NULL, ..., FUN=pred)
}

#' @importFrom rlang eval_tidy
add_top_level_internal <- function(N = NULL, ID_label = NULL,
                               workspace = NULL,
                               data_arguments = NULL) {

  # check_add_level_args(data_arguments, ID_label)


  # Check to make sure the N here is sane
  N <- handle_n(N, add_level = TRUE, workspace)

  # If the user already has a working data frame, we need to shelf it before
  # we move on.


  working_data_list <- list()



  # Staple in an ID column onto the data list.

  # It's actually not possible the working data frame already has an ID label
  # since we forcibly shelved it earlier -- so let's just plough along.

  # First, add the column to the working data frame
  working_data_list[[ID_label]] <- generate_id_pad(N)


  # check_variables_named(data_arguments)

  # Loop through each of the variable generating arguments
  for (i in seq_along(data_arguments)) {
    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    tmp <- expand_or_error(eval_tidy(
      data_arguments[[i]],
      append(working_data_list, list(N = N))
    ), N, i, data_arguments[[i]])

    if(names(data_arguments)[i] != "") {
      working_data_list[[names(data_arguments)[i]]] <- tmp
    } else {
      for(j in seq_along(tmp)) {
        working_data_list[[names(tmp)[j]]] <- tmp[[j]]
      }
    }

    # Nuke the current data argument -- if we have the same variable name
    # created twice, this is OK, because it'll only nuke the current one.
    # data_arguments[[i]] <- NULL
  }

  # Before handing back data, ensure it's actually rectangular
  working_data_list <- check_rectangular(working_data_list, N)

  # Coerce our working data list into a working data frame
  workspace[[ID_label]] <- data.frame(
    working_data_list,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  attr(workspace, "active_df") <- ID_label


  workspace
}


check_add_level_args <- function(data_arguments, ID_label) {
  if(any(names(data_arguments) == "")) {
    stop("All variables inside an add_level call must be named.")
  }

  if(is.null(ID_label) || is.na(ID_label)) {
    stop("Please specify a name for the level call you are creating.")

  }
}

make_list <- function(x, nm) {
  if(!is.list(x)){
    x <- list(x)
    names(x) <- nm
  }
  x
}

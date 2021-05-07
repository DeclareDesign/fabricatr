#' @importFrom rlang quos get_expr quo_text enquo
#'
#' @rdname fabricate
#' @export
nest_level <- function(N = NULL, ...) {
  do_internal(N, ..., FUN = nest_level_internal, from="nest_level")
}

#' @importFrom rlang eval_tidy
#'
nest_level_internal <- function(N = NULL, ID_label = NULL,
                                workspace = NULL,
                                data_arguments = NULL) {

  df <- active_df(workspace)


  # Check to make sure we have a data frame to nest on.
  if (is.null(dim(df))) {
    stop(
      "`nest_level()`` cannot be the top of a hierarchy. Use `add_level()`",
      "to either import or create a top level."
    )
  }

  # Check to make sure the N here is sane
  # Pass the working environment because N might not be a singleton here
  N <- handle_n(
    N, add_level = FALSE,
    working_environment = workspace,
    parent_frame_levels = 3
  )

  # We need to expand the size of the current working data frame by copying it
  # Let's start by getting the size of the current working data frame
  past_level_N <- nrow(df)
  # And now make an index set 1:past_level_N
  indices <- seq_len(past_level_N)

  # We're now going to modify the index set to take into account the expansion
  # If N is a single number, then we repeat each index N times
  # If N is of length past_level_N, then we repeat each index N_i times.
  # For r's rep, the each / times arguments have odd behaviour that
  # necessitates this approach
  if (length(N) == 1) {
    rep_indices <- rep(indices, each = N)
  } else {
    rep_indices <- rep(indices, times = N)
  }

  # Update N to the new length.
  inner_N <- N # Length specified for this level
  N <- length(rep_indices) # Length of overall data frame

  # stretch the data frame
  working_data_list <- as.list(df[rep_indices, , drop=FALSE])

  # Everything after here is non-unique to nest_level versus add_level -- need
  # to think about how to refactor this out.

  # Staple in an ID column onto the data list.
  if (!ID_label %in% names(working_data_list)) {
    # First, add the column to the working data frame
    working_data_list[[ID_label]] <- generate_id_pad(N)
  }

  # check_variables_named(data_arguments)

  # Loop through each of the variable generating arguments
  for (i in seq_along(data_arguments)) {

    nm <- names(data_arguments)[i]

    # Explicity mask N
    dm <- as_data_mask(working_data_list)
    dm$N <- N

    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    variable_data <- eval_tidy(
      data_arguments[[i]],
      dm
    )

    # User provided a fixed-length data variable whose length is the length of
    # the inner-most level for a given outer level. See example:
    # fabricate(countries = add_level(N=20),
    #           cities = nest_level(N=2, capital=c(TRUE, FALSE)))
    # We need to expand this to each setting of the outer level.
    # Only evaluate if inner_N is a single number

    is_vector <- nm != ""

    if(is_vector){

      # vector: length inner_N
      if (length(inner_N) == 1 && length(variable_data) == inner_N) {
        variable_data <- rep(variable_data, (N / inner_N))
      }
      # vector: length one
      if(length(variable_data) == 1) {
        variable_data <- rep(variable_data, N)
      }

      working_data_list[[nm]] <- variable_data

      if (length(working_data_list[[nm]]) != N) {
        stop(
          "Nested data length for the variable \"", i, "\" ",
          "appears to be incorrect. Nested data must either inherit the length ",
          "N or be fixed-length variables equal to the total number of ",
          "observations at the outer level. (In this case, ", N, "). Variable ",
          "supplied was length ", length(working_data_list[[nm]]), "\n\n"
        )
      }

    } else {
      # data.frame: length inner N
      if(nrow(variable_data) == inner_N) {
        variable_data <- variable_data[rep(seq_len(nrow(variable_data)), each = N / inner_N), , drop = FALSE]
      }

      # data.frame: length one
      if(nrow(variable_data) == 1) {
        variable_data <- variable_data[rep(seq_len(nrow(variable_data)), each = N), , drop = FALSE]
      }

      if (nrow(variable_data) != N) {
        stop(
          "Nested data length for the variable \"", i, "\" ",
          "appears to be incorrect. Nested data must either inherit the length ",
          "N or be fixed-length variables equal to the total number of ",
          "observations at the outer level. (In this case, ", N, "). Variable ",
          "supplied was length ", nrow(variable_data), "\n\n"
        )
      }

      for(j in seq_along(variable_data)) {
        working_data_list[[names(variable_data)[j]]] <- variable_data[[j]]
      }

    }

    # Nuke the current data argument -- if we have the same variable name
    # created twice, this is OK, because it'll only nuke the current one.
    # data_arguments[[nm]] <- NULL
  }

  # Before handing back data, ensure it's actually rectangular -- although
  # this should be covered by the error message above.
  working_data_list <- check_rectangular(working_data_list, N)

  append_child(workspace, child=ID_label, child_df=working_data_list)


  activate(workspace, ID_label)
}

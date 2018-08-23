#' @param by (optional) quoted name of variable \code{modify_level} uses to split-modify-combine data by.
#'
#' @importFrom rlang quos get_expr
#'
#' @rdname fabricate
#' @export
modify_level <- function(..., by=NULL) {
  do_internal(N=NULL, ..., by=by, FUN=modify_level_internal, from="modify_level")
}

#' @importFrom rlang eval_tidy
#'
modify_level_internal <- function(N = NULL, ID_label = NULL,
                                  workspace = NULL, by = NULL,
                                  data_arguments=NULL) {


  modify_level_internal_checks(ID_label, workspace)

  uu <- ID_label %||% attr(workspace, "active_df")

  df <- workspace[[uu]] %||% active_df(workspace)



  # There are two possibilities. One is that we are modifying the lowest level
  # of data. In which case, we simply add variables, like if someone called
  # add_level with a dataset. To check if that's the world we're in, check if
  # we have any duplicates in the ID label:
  if (!is.character(by)) {
    # There is no subsetting going on, but modify_level was used anyway.
    N <- nrow(df)

    # Coerce the working data frame into a list
    working_data_list <- as.list(df)


    check_variables_named(data_arguments, "modify_level")

    # Now loop over the variable creation.
    for (i in names(data_arguments)) {
      # Explicity mask N
      dm <- as_data_mask(working_data_list)
      dm$N <- N

      working_data_list[[i]] <- expand_or_error(eval_tidy(
        data_arguments[[i]],
        dm
      ), N, i, data_arguments[[i]])


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

    attr_names <- grep("^fabricatr::", names(attributes(df)), value = TRUE)
    attributes(workspace[[uu]])[attr_names] <- attributes(df)[attr_names]

    activate(workspace, uu)
    # Return results
    return(workspace)
  }


  idx <- split(seq_len(nrow(df)), df[by], drop = TRUE)


  for(slice in idx) {
    wenv <- import_data_list(df[slice, ,drop=FALSE])

    wenv <- modify_level_internal(N, ID_label, wenv, data_arguments=data_arguments)

    ret <- active_df(wenv)

    # If new columns were created, preallocate them, ow will be ignored w/ a warning
    df[setdiff(names(ret), names(df))] <- NA

    df[slice, names(ret)] <- ret

  }

  workspace[[uu]] <- df

  activate(workspace, uu)
  workspace

}




modify_level_internal_checks <- function(ID_label, workspace) {
  # Need to supply an ID_label, otherwise we have no idea what to modify.
  if (is.null(ID_label)) {
    stop(
      "You can't modify a level without a known level ID variable. If you ",
      "are trying to add nested data, please use `add_level()`"
    )
  }

  # First, establish that if we have no working data frame, we can't continue
  if (is.null(dim(active_df(workspace)))) {
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

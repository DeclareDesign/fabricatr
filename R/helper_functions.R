#' Expands data to a given length through recycling.
#'
#' This function is a helper function designed call `rep_len` to expand the
#' length of a data vector, but which can dynamically retrieve N from the
#' surrounding level call for use in fabricatr.
#'
#' @param x Data to recycle into length `N`
#' @param .N the length to recycle the data to, typically provided implicitly by
#' a or fabricate call wrapped around the function call.
#' @return A vector of data padded to length `N`
#' @export
recycle <- function(x, .N = NULL) {
  if(is.null(.N)) {
    .N <- tryCatch({
      dynGet("N")
    }, error = function(e) {
      NULL
    })

    if(is.null(.N)) {
      stop("You must supply a `.N` argument to `recycle` or run ",
           "`recycle` inside a level call to implicit supply `.N`.")
    }
  }

  rep_len(x, length.out = .N)
}

makeUU <- function() {
  sprintf("%X-%X", as.integer(Sys.time()), sample.int(.Machine$integer.max, 1))
}


import_data_list <- function(data) {
  workspace <- new_environment()

  if(is.null(data)) return(workspace)

  if(is.data.frame(data) || !is.list(data)) data <- list(data)

  for(df in data) {
    # Sanity check that the data we're bringing in is good.
    df <- handle_data(data = df)

    # Shelf the current working data if there's any.
    workspace <- shelf_working_data(workspace)

    uu <- makeUU()

    workspace[[uu]] <- df

    # Now copy the current data into the environment
    workspace$data_frame_output_ <- df
    workspace$variable_names_ <- names(df)

  }

  structure(workspace, active_df=uu, insertion_order=c())
}

shelf_working_data <- function(working_environment_) {
  what <- c(
    "data_frame_output_",
    "level_ids_",
    "variable_names_")

  if (exists("data_frame_output_", working_environment_)) {
    # Construct the shelved version
    package_df <- mget(what, working_environment_, ifnotfound = list(NULL))
    package_df <- filter_out_nulls(package_df)

    # Append it to the existing shelf
    working_environment_$shelved_df <- append(
      working_environment_$shelved_df,
      list(package_df)
    )

    # Clear the current work-space.
    rm(envir = working_environment_, list=names(package_df)) # rm warns if list has an element not in envir
  }

  working_environment_
}


#' @importFrom rlang is_quosure
get_symbols_from_quosures <- function(quosures) {

  extract <- function(l_arg) {
    # We have some sort of language expression in R, let's extract
    # the symbols it's going to refer to

    if (is_quosure(l_arg)){
      extract(get_expr(l_arg)) # extract from expression
    } else if (is.symbol(l_arg)) {
      # If it's a symbol, return the symbol as character
      as.character(l_arg)
    } else if (is.language(l_arg)) {
      # If it's a language call, then we need to unpack some more
      # Extract the args from the call, (drop names on arguments)
      recurse <- unname(lang_args(l_arg))
      # For each arg, extract
      temp <- lapply(recurse, extract)
      unlist(temp)
    } else {
      # It's something else? This might happen if the base level call
      # is numeric or whatever. We are only interested in variable nanes.
    }
  }

  # For each quosure, what symbols will that quosure attempt to read when it
  # is evaluated?
  meta_results <- lapply(quosures, extract)

  # remove duplicates
  meta_results <- Reduce(union, meta_results)

  return(meta_results)
}


#' Find which variables are unique at a given level in hierarchical data
#'
#' @param data a data.frame
#' @param ID_label the ID label to split upon
#' @param superset Superset contains a vector of character strings that contain variables
#' the modify level call is going to write. Some of these may be columns
#' in the data frame, others might not be. If superset is specified,
#' then we definitely only want to check those variables
#'
#' @return a character vector enumerating the unique variables
#' @keywords internal
get_unique_variables_by_level <- function(data, ID_label, superset=NULL) {
  if (is.character(superset)) {
    names_to_check <- intersect(colnames(data), superset)
  } else {
    names_to_check <- setdiff(colnames(data), ID_label)
  }

  if (is_empty(names_to_check)) return(names_to_check)

  # Iterate through each column of interest
  # Per column, split that column's data into a list. The split indices come
  # from the level indicator. Now, run a function which checks the unique
  # length of each tranch. Unlist the result to get a vector of TRUE or FALSE
  # for each tranch of the list. If all tranches are TRUE, then the column has
  # unique values based on the level's level. Take the results per column,
  # unlist those, strip the names (if any) from the variables. Now extract the
  # column names for the columns for which this was true. Return as a vector.

  # Performance is around 22% faster than existing code for small dataset
  level_variables <-
    vapply(
      data[names_to_check],
      function(x) {
        all(
          vapply(
            split(x, data[, ID_label]),
            function(x) {
              length(unique(x)) == 1
            },
            FALSE
          )
        )
      },
      FALSE
    )

  names_to_check[level_variables]
}


# Checks if an ID label is sane, warns or errors if not.
# Generates an ID label if there isn't one provided.
handle_id <- function(ID_label, data=NULL) {
  # If the user passed a symbol, we should evaluate the symbol forcibly and
  # error if they were assuming NSE substitution of an undefined symbol.
  tryCatch(
    force(ID_label),
    error = function(e) {
      stop(
        "The `ID_label` provided is a reference to an undefined variable. ",
        "Please enclose `ID_label` in quotation marks if you intended to ",
        "provide `ID_label` as a character vector."
      )
    }
  )

  # User passed a non-symbol non-null ID_label
  if (is.vector(ID_label)) {
    if (length(ID_label) != 1) {
      stop("Provided `ID_label` must be a string.")
    } else if (is.numeric(ID_label)) {
      # Numeric ID_label -- this is OK but variable names can't be numeric
      warning("Provided `ID_label` is numeric and will be prefixed with \"X\"")
      ID_label <- as.character(ID_label)
    }
  }

  # Higher dimensional ID_label
  if (!is.null(dim(ID_label))) {
    stop("Provided `ID_label` must be a character vector or variable name, not a data frame or matrix.")
  }

  if(!is.null(ID_label)) return(ID_label)

  # At the end of all this, we still don't have an ID label
  if (!"ID" %in% names(data)) return("ID")

  # "ID" is taken, so we're going to try some backups

  for(candidate_label in setdiff(paste0("fab_ID_", 1:5), names(data))) {
    return(candidate_label)
  }

  stop(
    "No `ID_label` specified for level and supply of default ID ",
    "labels -- ID, fab_ID_1, fab_ID_2, fab_ID_3, fab_ID_4, fab_ID_5",
    " -- are all used for data columns. Please specify an `ID_label` ",
    "for this level."
  )
}

# Checks if a supplied N is sane for the context it's in
handle_n <- function(N, add_level=TRUE, working_environment, parent_frame_levels=1) {
  # Error handling for user-supplied N

  df <- active_df(working_environment)
  # First, evaluate the N in the context of the working environment's working
  # data frame. Why do we need to do this? Because N could be a function of
  # variables.
  N <- eval_tidy(N, data = df)

  # User provided an unevaluated function
  if (typeof(N) == "closure") {
    stop("`N` must not be a function.")
  }

  if (!is_integerish(N))
    stop("Provided `N` must be a single positive integer.")

  if(add_level && !is_scalar_integerish(N))
        stop("When adding a new level, the specified `N` must be a single number.")

  if (length(N) > 1) {
    # User specified more than one N; presumably this is one N for each
    # level of the last level variable

    # What's the last level variable?
    last_level_name <- attr(working_environment, "prev_df")

    # Last level name is null; if this is imported data, we should
    # use the nrow of the data frame as the unique length of the last
    # level
    if(is.null(last_level_name)) {
      last_level_name <- "the full data frame"
      length_unique <- nrow(df)
    } else {
      # What are the unique values?
      unique_values_of_last_level <- unique(
        df[[last_level_name]]
      )
      length_unique <- length(unique_values_of_last_level)
    }


    if (length(N) != length_unique) {
      stop(
        "`N` must be either a single number or a vector of length ",
        length_unique,
        " (one value for each possible level of ",
        last_level_name,
        ")"
      )
    }
  }



  N
}

# Checks if the user-provided data is sane
# errors if not.
handle_data <- function(data) {
  if (!is.null(data) & !missing(data) & !"data.frame" %in% class(data)) {
    # User provided data, but it's not 2D
    if (is.null(dim(data))) {
      stop(
        "User provided `data` must be a data frame. Provided `data` was low ",
        "dimensional."
      )
    }

    # User provided data, but it's not a data frame, and they didn't provide
    # it explicitly, so this is probably a mess-up with an implicit argument
    if (!"data" %in% names(sys.call()) &&
      !"data" %in% names(sys.call(-1))) {
      stop(
        "The `data` argument must be a data object. The argument call, ",
        deparse(substitute(data)),
        ", was not a data object (e.g. a data.frame, tibble, sf object, or ",
        "convertible matrix)."
      )
    }

    # Convert user data to a data frame
    tryCatch({
      data <- data.frame(data, stringsAsFactors = FALSE)
    }, error = function(e) {
      # We can't make it a data frame -- this should probably never happen,
      # since it relies on something with a dim attribute not converting to
      # a data frame.
      stop(
        "User provided `data` could not convert to a data frame."
      )
    })
  }
  return(data)
}

# Function to check if something is a level call
call_not_level_call <- function(calls) {
  vapply(calls,
         function(i) {
           if(is_lang(get_expr(i))) {
             return(!lang_name(i) %in% c("level", "add_level",
                                         "nest_level", "modify_level",
                                         "cross_levels", "link_levels"))
           } else {
             return(TRUE)
           }
         },
         logical(1))
}



# Function to check if every argument in a quosure options
# is a level call.
check_all_levels <- function(options) {
  # Passing the options quosures
  # There were no levels, or indeed arguments, at all
  if (length(options) == 0) return(FALSE)

  # get_expr returns the expression for an item in a quosure
  # is_lang checks if it's a function
  is_function <- vapply(options,
                        function(i) {
                          is_lang(get_expr(i))
                        },
                        FALSE)

  # lang_name gets function name from a quosure
  func_names <- vapply(options[is_function], lang_name, "")

  # Check to see if the function names are one of the valid level operations
  is_level <- func_names %in% c(
      "level",
      "add_level",
      "nest_level",
      "modify_level",
      "cross_levels",
      "link_levels"
    )

  # Return false if we have no level calls
  if (!any(is_level)) return(FALSE)

  # If some calls are levels and some aren't, we're unhappy
  if (!all(is_level)) {
    stop(
      "Arguments passed to `...` must either all be calls to create or modify ",
      "levels, or else none of them must be."
    )
  }

  # Confirm they're all levels
  length(is_level) == length(options)
}



# Generates IDs from 1:N with zero left padding for visual display.
generate_id_pad <- function(N,zero=c("0", "")) {
  sprintf(paste0("%", match.arg(zero), nchar(N), "d"), 1:N)
}



#' @importFrom rlang f_rhs
expand_or_error <- function(vector_data, N, variable_name, call_string) {
  # NULL data means deleting a variable -- this is OK
  if(is.null(vector_data)) { return(NULL) }
  vector_dims <- dim(vector_data)
  if(length(vector_dims) > 1) {
    if(vector_dims[1] == N){
      return(vector_data)
    }
    else {
      stop(simpleError(paste0("Nested structures must have `N.` rows ",
                              "In this call, `N` = ", N, " while the variable ",
                              variable_name, " is length ", vector_dims[1]),
                       call = f_rhs(call_string)))

    }

  }


  # Error if it's neither N nor 1
  if(!length(vector_data) %in% c(1, N)) {
    stop(simpleError(paste0("Variable lengths must all be equal to `N.` ",
                            "In this call, `N` = ", N, " while the variable ",
                            variable_name, " is length ", length(vector_data)),
                     call = f_rhs(call_string)))
  }

  # Recycle if it's 1, if not return data as-is.
  if(length(vector_data) == 1) { return(rep(vector_data, N)) }
  else { return(vector_data) }
}



# Try to overwrite R's recycling of vector operations to ensure the initial
# data is rectangular -- needs an N to ensure that constants do get recycled.
check_rectangular <- function(working_data_list, N) {

  for (i in seq_along(working_data_list)) {
    wdl_i <- working_data_list[[i]]
    d <- dim(wdl_i)
    if(length(d)  %in% 0:1) {
      len <- length(wdl_i)
      if (len == 1) {
        # Variable is a constant -- repeat it N times
        working_data_list[[i]] <- rep_len(wdl_i, N)
      } else if (len != N) {
        # Variable is not of length N. Oops.
        stop("Variables  must all be length `N.` ",
             "In this call, `N` = ", N, " while the variable `",
             names(working_data_list)[i], "` is length ", len)
      }
    }
    else if(length(d) == 2){
      if(d[1] != N) {
        stop("Nested structures must all have `N.` rows. ",
             "In this call, `N` = ", N, " while the variable `",
             names(working_data_list)[i], "` has ", d[1], " rows.")


      }

    }
  }
  return(working_data_list)
}



# Add a level ID to a working environment
add_level_id <- function(working_environment_, ID_label) {
  # Add or create level ID list
  attr(working_environment_, "prev_df") <- ID_label
}



# Add a variable name to a working environment
add_variable_name <- function(working_environment_, variable_name) {
  working_environment_$variable_names_ <- append(
    working_environment_$variable_names_,
    variable_name
  )
}


do_internal <- function(N = NULL, ..., FUN, from, by = NULL, nest = NULL) {
  dots <- quos(...)
  if(!has_name(dots, "working_environment_")){
    # This happens if either call is run external to a fabricate
    # call OR if add_level is the only argument to a fabricate call and
    # the data argument tries to resolve an add_level call.
    stop(
      "`", from, "()` calls must be used inside `fabricate()` calls."
    )
  }

  working_environment_ <- get_expr(dots[["working_environment_"]])
  dots[["working_environment_"]] <- NULL


  if (has_name(dots, "ID_label")) {
    ID_label <- get_expr(dots[["ID_label"]])
    dots[["ID_label"]] <- NULL
  }

  # worse is better :()
  if(has_name(formals(FUN), "by")){
    FUN(
      N = N, ID_label = ID_label, by = by,
      working_environment_ = working_environment_,
      data_arguments = dots
    )
  } else if (has_name(formals(FUN), "nest")){
    FUN(
      N = N, ID_label = ID_label,
      working_environment_ = working_environment_,
      data_arguments = dots,
      nest = nest
    )
  } else {
    FUN(
      N = N, ID_label = ID_label,
      working_environment_ = working_environment_,
      data_arguments = dots
    )
  }
}


# Dummy helper function that just extracts the working data frame from the
# environment. This exists because we may in the future want to return something
# that is not a data frame.
report_results <- active_df <- function(workspace) {
  uu <- attr(workspace, "active_df")
  if(is.null(uu)) NULL else workspace[[uu]]
}

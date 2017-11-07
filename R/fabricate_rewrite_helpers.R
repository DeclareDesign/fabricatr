# Checks if an ID label is sane, warns or errors if not.
# Generates an ID label if there isn't one provided.
handle_id = function(ID_label, data=NULL) {
  # User passed a non-symbol non-null ID_label
  if(!is.null(ID_label)) {
    if(is.vector(ID_label) & length(ID_label) > 1) {
      # Vector of length n>1, error
      stop("Provided ID_label must be a character vector of length 1 or variable name.")
    } else if(is.vector(ID_label) & is.numeric(ID_label[1])) {
      # Numeric ID_label -- this is OK but variable names can't be numeric
      warning("Provided ID_label is numeric and will be prefixed with the character \"X\"")
      ID_label <- as.character(ID_label)
    } else if(is.vector(ID_label) & is.character(ID_label[1])) {
      # Valid ID_label
      ID_label <- as.character(ID_label)
    } else if(!is.null(dim(ID_label))) {
      # Higher dimensional ID_label
      stop("Provided ID_label must be a character vector or variable name, not a data frame or matrix.")
    }
  }

  # At the end of all this, we still don't have an ID label
  if(is.null(ID_label)) {
    if(is.null(data) | missing(data)) {
      ID_label = "ID"
    } else {
      # We need to come up with an ID, but there's some data, so we're not sure
      tries = 0
      # "ID" isn't taken
      if (!"ID" %in% names(data)) {
        ID_label = "ID"
      } else {
        # "ID" is taken, so we're going to try some backups
        while(tries < 5) {
          tries = tries + 1
          candidate_label = paste0("fab_ID_", tries)
          # This backup is available
          if(!candidate_label %in% names(data)) {
            ID_label = candidate_label
            break
          }
        }

        # We tried all our backup IDs and still couldn't find a valid ID
        if(tries >= 5 & is.null(ID_label)) {
          stop(
            "No ID label specified for level and supply of default ID_labels -- ID, fab_ID_1, fab_ID_2, fab_ID_3, fab_ID_4, fab_ID_5 -- all used for data columns. Please specify an ID_label for this level."
          )
        }
      }
    }
  }

  # Return the resulting ID_label
  return(ID_label)
}

# Checks if a supplied N is sane for the context it's in
handle_n = function(N, add_level=TRUE, working_environment=NULL) {
  # Error handling for user-supplied N

  # If they provided an N
  if(!is.null(N)) {
    # If this is an add_level operation, N must be a single number
    if(add_level) {
      if(length(N) > 1) {
        stop(
          "When adding a new level, the specified N must be a single number."
        )
      }
    } else {
      if(length(N) > 1) {
        # User specified more than one N; presumably this is one N for each level of the
        # last level variable

        # What's the last level variable?
        name_of_last_level = working_environment[["level_ids_"]][length(
          working_environment[["level_ids_"]])]

        # What are the unique values?
        unique_values_of_last_level = unique(
          working_environment[["data_frame_output_"]][[name_of_last_level]]
        )

        if(length(N) != length(unique_values_of_last_level)) {
          stop(
            "N must be either a single number or a vector of length ",
            length(unique_values_of_last_level),
            " (one value for each possible level of ",
            name_of_last_level,
            ")"
          )
        }
      }
      # If this is not an add_level operation, there are other options

    }

    # If any N is non-numeric or non-integer or negative or zero, fail.
    if(is.numeric(N) & any(N%%1 | N<=0)) {
      stop(
        "Provided N must be a single positive integer."
      )
    }

    # Coerce to numeric or fail
    if(!is.numeric(N)) {
      tryCatch({
        N = as.numeric(N)
      }, error=function(e) {
        stop(
          "Provided values for N must be integer numbers"
        )
      })
    }
  }
}

# Checks if the user-provided data is sane
# errors if not.
handle_data = function(data) {
  if(!is.null(data) & !missing(data) & !"data.frame" %in% class(data)) {
    # User provided data, but it's not 2D
    if(is.null(dim(data))) {
      stop(
        "User provided data must be a data frame. Provided data was low dimensional."
      )
    }

    # User provided data, but it's not a data frame, and they didn't provide it explicitly,
    # so this is probably a mess-up with an implicit argument
    if(!"data" %in% names(sys.call())) {
      stop(
        "The data argument must be a data object. The argument call, ", deparse(substitute(data)), ", was not a data object (e.g. a data.frame, tibble, sf object, or convertible matrix)."
      )
    }

    # Convert user data to a data frame
    tryCatch({
      data = as.data.frame(data)
    }, error=function(e) {
      # We can't make it a data frame -- this should probably never happen,
      # since it relies on something with a dim attribute not converting to
      # a data frame.
      stop(
        "User provided data could not convert to a data frame."
      )
    })
  }
  return(data)
}

# Function to check if every argument in a quosure options
# is a level call.
check_all_levels_new <- function(options){
  # Passing the options quosures
  # There were no levels, or indeed arguments, at all
  if (length(options) == 0)  return(FALSE)

  # get_expr returns the expression for an item in a quosure
  # is_lang checks if it's a function
  is_function <- sapply(options, function(i) {
    is_lang(get_expr(i))
  })

  # lang_name gets function name from a quosure
  func_names = sapply(options[is_function], lang_name)

  # Check to see if the function names are one of the valid level operations
  is_level = sapply(func_names, function(i) { i %in% c("level",
                                                       "add_level_new",
                                                       "nest_level_new",
                                                       "modify_level_new") })

  # Return false if we have no level calls
  if(length(is_level) == 0) return(FALSE)

  # If some calls are levels and some aren't, we're unhappy
  if (any(is_level) != all(is_level)) {
    stop(
      "Arguments passed to ... must either all be calls to create or modify levels, or else none of them must be."
    )
  }

  # Confirm they're all levels
  is_level[1] && length(is_level) == length(options)
}


# Generates IDs from 1:N with zero left padding for visual display.
generate_id_pad <- function(N){
  # Left-Pad ID variable with zeroes
  format_left_padded <- paste0("%0", nchar(N), "d")

  # Add it to the data frame.
  return(sprintf(format_left_padded, 1:N))
}

# Try to overwrite R's recycling of vector operations to ensure the initial
# data is rectangular -- needs an N to ensure that constants do get recycled.
check_rectangular = function(working_data_list, N) {
  for(i in seq_along(working_data_list)) {
    if(length(working_data_list[[i]]) == 1) {
      # Variable is a constant -- repeat it N times
      working_data_list[[i]] = rep(working_data_list[[i]], N)
    } else if(length(working_data_list[[i]]) != N) {
      # Variable is not of length N. Oops.
      stop("Variable lengths must all be equal to N.")
    }
  }
  return(working_data_list)
}

# Add a level ID to a working environment
add_level_id = function(working_environment_, ID_label) {
  # Add or create level ID list
  if("level_ids_" %in% names(working_environment_)) {
    working_environment_[["level_ids_"]] = append(working_environment_[["level_ids_"]], ID_label)
  } else {
    working_environment_[["level_ids_"]] = c(ID_label)
  }

  return(working_environment_)
}

# Add a variable name to a working environment
add_variable_name = function(working_environment_, variable_name) {
  # Add or create variable name list.
  if("variable_names_" %in% names(working_environment_)) {
    working_environment_[["variable_names_"]] = append(working_environment_[["variable_names_"]], variable_name)
  } else {
    working_environment_[["variable_names_"]] = c(variable_name)
  }

  return(working_environment_)
}

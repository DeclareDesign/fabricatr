#' Helper function (NOTE: THIS FILE IS DEPRECATED!!!!!!)
#' @importFrom rlang quos lang_args
#'
get_symbols_from_expression = function(l_arg) {
  # We have some sort of language expression in R, let's extract
  # the symbols it's going to refer to

  if(is.symbol(l_arg)) {
    # If it's a symbol, return the symbol
    return(unname(l_arg))
  } else if(is.language(l_arg)) {
    # If it's a language call, then we need to unpack some more
    # Extract the language from the language call
    recurse = lang_args(l_arg)
    # Iterate through each part of the language, recursively calling this function
    # Results are a list, so unlist and unname to flatten
    temp = unname(unlist(lapply(recurse, function(i) { get_symbols_from_expression(i) })))
    return(temp)
  } else {
    # It's something else? This might happen if the base level call
    # is numeric or whatever. We are only interested in variable nanes.
  }
}

#'
#' @importFrom rlang quos lang_args get_expr
#'
get_symbols_from_quosure = function(quosure) {
  # Given a quosure, what symbols will that quosure attempt to read when it
  # is evaluated?
  meta_results =   lapply(quosure, function(i) {
    # For each term in the quosure, get the language call out of the term:
    expression = get_expr(i)
    # Get the arguments out of that language call
    thing = lang_args(expression)
    # Now, for each argument try to extract the symbols
    results = lapply(thing, function(x) { get_symbols_from_expression(x) })

    # We are going to unlist, convert to characters (this is necessary to coerce
    # results into a vector), and then remove duplicates
    return(unique(
      as.character(
        unlist(
          results))))
  })

  return(meta_results)
}

get_unique_variables_by_level <- function(data, ID_label, superset=NULL) {
  # Superset contains a vector of character strings that contain variables
  # the modify level call is going to write. Some of these may be columns
  # in the data frame, others might not be. If superset is specified,
  # then we definitely only want to check those variables
  if(!is.null(superset)) {
    names_to_check = intersect(colnames(data), superset)
  } else {
    names_to_check = colnames(data)[-which(colnames(data)==ID_label)]
  }

  # It turns out the call isn't going to use any variables at all!
  if(!length(names_to_check)) { return("") }

  # Iterate through each column of interest
  # Per column, split that column's data into a list. The split indices come from the level indicator.
  # Now, run a function which checks the unique length of each tranch
  # Unlist the result to get a vector of TRUE or FALSE for each tranch of the list.
  # If all tranches are TRUE, then the column has unique values based on the level's level.
  # Take the results per column, unlist those, strip the names (if any) from the variables.
  # Now extract the column names for the columns for which this was true. Return as a vector.

  # Performance is around 22% faster than existing code for small dataset
  level_variables = names_to_check[
    unname(unlist(lapply(names_to_check,
                               function(i) {
                                 all(unlist(
                                   lapply(
                                     split(data[, i], data[, ID_label]),
                                     function(x) {
                                       length(unique(x))==1
                                      }
                                    )
                                  ))
                                }
                               )
                        ))
    ]
  return(level_variables)
}

expand_data_by_ID <- function(data, ID_label, N) {
  if (typeof(N) %in% c("integer", "double") &&
      length(N) == 1) {
    data <- data[rep(1:nrow(data), each = N), , drop = FALSE]
  } else if (typeof(N) %in% c("integer", "double") &&
             length(N) > 1) {
    # check that the vector that is N is the right length, i.e the length of data_internal_
    if (length(N) != nrow(data)) {
      stop(
        paste0(
          "If you provide a vector to N for level ",
          ID_label,
          ", it must be the length of the dataset at the level above it ",
          "in the hierarchy"
        )
      )
    }
    data <- data[rep(1:nrow(data), times = N), , drop = FALSE]
  } else if (class(N) == "function") {
    data <- data[rep(1:nrow(data), times = N()), , drop = FALSE]
  } else {
    stop(
      paste0(
        "Please provide level ",
        ID_label,
        " with N that is a vector, scalar, or function that generates a vector."
      )
    )
  }
}

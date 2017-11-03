#' Fabricate data
#'
#' \code{fabricate} helps you simulate a dataset before you collect it. You can either start with your own data and add simulated variables to it (by passing \code{data} to \code{fabricate()}) or start from scratch by defining \code{N}. Create hierarchical data with multiple levels of data such as citizens within cities within states using \code{level()}. You can use any R function to create each variable. We provide several built-in options to easily draw from binary and count outcomes, \code{\link{draw_binary}} and \code{\link{draw_discrete}}.
#'
#' @param data (optional) user-provided data that forms the basis of the fabrication, i.e. you can add variables to existing data. Provide either \code{N} or \code{data} (\code{N} is the number of rows of the data if \code{data} is provided).
#' @param N (optional) number of units to draw. If provided as \code{fabricate(N = 5)}, this determines the number of units in the single-level data. If provided in \code{level}, i.e. \code{fabricate(cities = level(N = 5))}, \code{N} determines the number of units in a specific level of a hierarchical dataset.
#' @param ID_label (optional) variable name for ID variable, i.e. citizen_ID
#' @param ... Variable or level-generating arguments, such as \code{my_var = rnorm(N)}. For \code{fabricate}, you may also pass \code{level()} arguments, which define a level of a multi-level dataset. See examples.
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
#'  regions = level(N = 5),
#'  cities = level(N = 2, pollution = rnorm(N, mean = 5)))
#' head(df)
#'
#' # Start with existing data and add variables to hierarchical data
#' # note: do not provide N when adding variables to an existing level
#' df <- fabricate(
#'   data = df,
#'   regions = level(watershed = sample(c(0, 1), N, replace = TRUE)),
#'   cities = level(runoff = rnorm(N))
#' )
#'
#' @importFrom rlang quos quo_name eval_tidy lang_name lang_modify lang_args is_lang get_expr
#'
#' @export
fabricate <- function(data = NULL, N = NULL, ID_label = NULL, ...)
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
      "Fabricate can be called in one of three ways: \n 1) Provide one or more level calls, with or without existing data \n 2) Provide existing data and add new variables without adding a level \n 3) Provide an \"N\" and add new variables"
      )
  }

  # User provided level calls
  if(all_levels) {
    # Ensure the user provided a name for each level call.
    if(is.null(names(data_arguments)) | any(names(data_arguments) == "")) {
      stop("You must provide a name for each level you create.")
    }

    # Create a blank working environment.
    working_environment = list()

    # User provided data, if any, should be preloaded into working environment
    if(!is.null(data) & !missing(data)) {
      working_environment[["imported_data_"]] = data
    }

    # Each of data_arguments is a level call
    for(i in seq_along(data_arguments)) {
      # Add two variables to the argument of the current level call
      # one to pass the working environment so far
      # one to pass the ID_label the user intends for the level
      data_arguments[[i]] = lang_modify(data_arguments[[i]],
                                        working_environment_ = working_environment,
                                        ID_label = names(data_arguments)[i])

      # Execute the level build and pass it back to the current working environment.
      working_environment = eval_tidy(data_arguments[[i]])
    }

    print(working_environment)

    # Return the results from the working environment
    return(report_results(working_environment))
  }

  # Single level
  # Sanity check and/or construct an ID label for the new data.
  ID_label = handle_id(ID_label, data)

  # User passed N, not data.
  if(is.null(data) | missing(data)) {
    # Is the N argument passed here sane? Let's check
    handle_n(N, add_level = TRUE)

    # Creating a working environment
    data_arguments[["working_environment_"]] = list()

    return(
      report_results(
        add_level(N = N, ID_label = ID_label, data_arguments = data_arguments)
      )
    )
  }

  # User passed data
  N = nrow(data)
  data_arguments[["working_environment_"]] = list(imported_data_ = data)

  return(
    report_results(
      add_level(N = N, ID_label = ID_label, data_arguments = data_arguments)
    )
  )
}

add_level = function(N = NULL, ID_label = NULL,
                     working_environment_ = NULL,
                     ...,
                     data_arguments=quos(...)) {

  # Copy the working environment out of the data_arguments quosure and into the root.
  if("working_environment_" %in% names(data_arguments)) {
    working_environment_ = data_arguments[["working_environment_"]]
    data_arguments[["working_environment_"]] = NULL
  }

  # Copy ID_label out of the data_arguments quosure and into the root
  if("ID_label" %in% names(data_arguments)) {
    ID_label = data_arguments[["ID_label_"]]
    data_arguments[["ID_label"]] = NULL
  }

  # Check to make sure the N here is sane
  handle_n(N, add_level=TRUE)


  # User is adding a new level, but already has a working data frame.
  # Shelf the working data frame and move on
  if("data_frame_output_" %in% names(working_environment_)) {
    # Construct the shelved version
    package_df = list(data_frame_output_ = working_environment_[["data_frame_output_"]],
                      level_ids_ = working_environment_[["level_ids_"]],
                      variable_names_ = names(working_environment_[["data_frame_output_"]]))

    # Append it to the existing shelf
    if("shelved_df" %in% names(working_environment_)) {
      working_environment_[["shelved_df"]] = append(working_environment_[["shelved_df"]], package_df)
    } else {
      # Create a shelf just for this
      working_environment_[["shelved_df"]][[1]] = package_df
    }

    # Clear the current work-space.
    working_environment_[["data_frame_output_"]] =
      working_environment_[["level_ids_"]] =
      working_environment_[["variable_names_"]] = NULL
  }

  # User is adding a new level, but we need to sneak in the imported data first.
  # When this is done, trash the imported data, because the working data frame contains it.
  if("imported_data_" %in% names(working_environment_)) {
    working_data_list = as.list(working_environment_[["imported_data_"]])
    working_environment_[["imported_data_"]] = NULL
  } else {
    working_data_list = list()
  }

  # Staple in an ID column onto the data list.
  if(!is.null(ID_label) && (is.null(names(working_data_list)) || !ID_label %in% names(working_data_list))) {
    # First, add the column to the working data frame
    working_data_list[[ID_label]] = generate_id_pad(N)

    # Next, add the ID_label to the level ids tracker
    if("level_ids_" %in% names(working_environment_)) {
      working_environment_[["level_ids_"]] = append(working_environment_[["level_ids_"]], ID_label)
    } else {
      working_environment_[["level_ids_"]][1] = ID_label
    }
  }

  # Loop through each of the variable generating arguments
  for(i in names(data_arguments)) {
    # Evaluate the formula in an environment consisting of:
    # 1) The current working data list
    # 2) A list that tells everyone what N means in this context.
    working_data_list[[i]] = eval_tidy(data_arguments[[i]],
                                       append(working_data_list, list(N=N)))

    # Write the variable name to the list of variable names
    if("variable_names_" %in% names(working_environment_)) {
      working_environment_[["variable_names_"]] = append(working_environment_[["variable_names_"]], i)
    } else {
      working_environment_[["variable_names_"]][1] = i
    }

    # Nuke the current data argument -- if we have the same variable name created twice,
    # this is OK, because it'll only nuke the current one.
    data_arguments[[i]] = NULL
  }

  # Before handing back data, ensure it's actually rectangular
  for(i in seq_along(working_data_list)) {
    if(length(working_data_list[[i]]) == 1) {
      # Variable is a constant -- repeat it N times
      working_data_list[[i]] = rep(working_data_list[[i]], N)
    } else if(length(working_data_list[[i]]) != N) {
      # Variable is not of length N. Oops.
      stop("Variable lengths must all be equal to N.")
    }
  }

  working_environment_[["data_frame_output_"]] = data.frame(working_data_list,
                                                            stringsAsFactors=FALSE,
                                                            row.names=NULL)

  return(working_environment_)
}

nest_level = function(N = NULL, ID_label = NULL,
                      working_environment_ = NULL,
                      ...,
                      data_arguments=quos(...)) {

  # Copy the working environment out of the data_arguments quosure and into the root.
  if("working_environment_" %in% names(data_arguments)) {
    working_environment_ = data_arguments[["working_environment_"]]
    data_arguments[["working_environment_"]] = NULL
  }

  # Copy ID_label out of the data_arguments quosure and into the root
  if("ID_label" %in% names(data_arguments)) {
    ID_label = data_arguments[["ID_label_"]]
    data_arguments[["ID_label"]] = NULL
  }

  # Check to make sure the N here is sane
  handle_n(N, add_level=TRUE)

}

modify_level = function(N = NULL, ID_label = NULL, ...) {
}

level = function(N = NULL, ID_label = NULL, ...) {

}

report_results = function(working_environment) {
  return(working_environment[["data_frame_output_"]])
}

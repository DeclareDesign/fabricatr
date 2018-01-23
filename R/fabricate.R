#' Fabricate data
#'
#' \code{fabricate} helps you simulate a dataset before you collect it. You can
#' either start with your own data and add simulated variables to it (by passing
#' \code{data} to \code{fabricate()}) or start from scratch by defining
#' \code{N}. Create hierarchical data with multiple levels of data such as
#' citizens within cities within states using \code{add_level()} or modify
#' existing hierarchical data using \code{modify_level()}. You can use any R
#' function to create each variable. Use \code{cross_level()} to make more
#' complex designs such as panel or cross-classified data.
#'
#' We also provide several built-in options to easily create variables, including
#' \code{\link{draw_binary}}, \code{\link{draw_count}}, \code{\link{draw_likert}},
#' and intra-cluster correlated variables \code{\link{draw_binary_icc}} and
#' \code{\link{draw_normal_icc}}
#'
#' @param data (optional) user-provided data that forms the basis of the
#' fabrication, e.g. you can add variables to existing data. Provide either
#' \code{N} or \code{data} (\code{N} is the number of rows of the data if
#' \code{data} is provided).
#' @param N (optional) number of units to draw. If provided as
#' \code{fabricate(N = 5)}, this determines the number of units in the
#' single-level data. If provided in \code{add_level}, e.g.
#' \code{fabricate(cities = add_level(N = 5))}, \code{N} determines the number
#' of units in a specific level of a hierarchical dataset.
#' @param ID_label (optional) variable name for ID variable, e.g. citizen_ID
#' @param ... Variable or level-generating arguments, such as
#' \code{my_var = rnorm(N)}. For \code{fabricate}, you may also pass
#' \code{add_level()} or \code{modify_level()} arguments, which define a level
#' of a multi-level dataset. See examples.
#' @param nest (Default TRUE) Boolean determining whether data in an
#' \code{add_level()} call will be nested under the current working data frame
#' or create a separate hierarchy of levels. See our vignette for
#' cross-classified, non-nested data for details.
#'
#' @return data.frame
#'
#' @examples
#'
#'
#' # Draw a single-level dataset with a covariate
#' building_df <- fabricate(
#'   N = 100,
#'   height_ft = runif(N, 300, 800)
#' )
#' head(building_df)
#'
#' # Start with existing data instead
#' building_modified <- fabricate(
#'   data = building_df,
#'   rent = rnorm(N, mean = height_ft * 100, sd = height_ft * 30)
#' )
#'
#' # Draw a two-level hierarchical dataset
#' # containing cities within regions
#' multi_level_df <- fabricate(
#'  regions = add_level(N = 5),
#'  cities = add_level(N = 2, pollution = rnorm(N, mean = 5)))
#' head(df)
#'
#' # Start with existing data and add a nested level:
#' company_df <- fabricate(
#'  data = building_df,
#'  company_id = add_level(N=10, is_headquarters = sample(c(0, 1), N, replace=TRUE))
#' )
#'
#' # Start with existing data and add variables to hierarchical data
#' # at levels which are already present in the existing data.
#' # Note: do not provide N when adding variables to an existing level
#' modified_multi_level_df <- fabricate(
#'   data = multi_level_df,
#'   regions = modify_level(watershed = sample(c(0, 1), N, replace = TRUE)),
#'   cities = modify_level(runoff = rnorm(N))
#' )
#'
#' # fabricatr can also make panel or cross-classified data. For more
#' # information about syntax for this functionality please read our vignette
#' # or check documentation for \code{cross_level}:
#' cross_classified <- fabricate(
#'   primary_schools = add_level(N = 50, ps_quality = runif(N, 0, 10)),
#'   secondary_schools = add_level(N = 100, ss_quality = runif(N, 0, 10), nest=FALSE),
#'   students = cross_level(N = 2000,
#'                          by=join(ps_quality, ss_quality, rho = 0.5),
#'                          student_quality = ps_quality + 3*ss_quality + rnorm(N)))
#' @seealso [cross_level()]
#' @importFrom rlang quos quo_name eval_tidy lang_name lang_modify lang_args
#' is_lang get_expr
#'
#' @export
fabricate <- function(data = NULL, ..., N = NULL, ID_label = NULL) {
  # Store all data generation arguments in a quosure for future evaluation
  # A quosure contains unevaluated formulae and function calls.
  data_arguments <- quos(...)

  # Fabricatr expects either a single-level function call
  # or a series of level calls. You can't mix and match.
  # This helper function will be TRUE if calls are all levels, FALSE
  # if there are no calls or they are not levels.
  all_levels <- check_all_levels(data_arguments)

  # User must provide exactly one of:
  # 1) One or more level calls (with or without importing their own data)
  # 2) Import their own data and do not involve level calls
  # 3) Provide an N without importing their own data
  if (sum(
    (!missing(data) && !is.null(data) & !all_levels),
    (!is.null(N) & !missing(N)),
    all_levels
  ) != 1) {
    stop(
      "You must do exactly one of: \n",
      "1) One or more level calls, with or without existing data \n",
      "2) Import existing data and optionally, add new variables without adding a level \n",
      "3) Provide an `N` without importing data and optionally, add new variables"
    )
  }

  # User provided level calls
  if (all_levels) {
    # Ensure the user provided a name for each level call.
    if (is.null(names(data_arguments)) | any(names(data_arguments) == "")) {
      stop("You must provide a name for each level you create.")
    }

    # User provided data, if any, should be preloaded into working environment
    if (!is.null(data) & !missing(data)) {
      working_environment <- import_data_list(data)
    } else {
      working_environment <- new_working_environment()
    }

    # Each of data_arguments is a level call
    for (i in seq_along(data_arguments)) {
      # Add two variables to the argument of the current level call
      # one to pass the working environment so far
      # one to pass the ID_label the user intends for the level

      data_arguments[[i]] <- lang_modify(
        data_arguments[[i]],
        working_environment_ = working_environment,
        ID_label = names(data_arguments)[i]
      )

      # Execute the level build and pass it back to the current working
      # environment.
      working_environment <- eval_tidy(data_arguments[[i]])
    }

    # Return the results from the working environment
    return(report_results(working_environment))
  }

  # User did not pass data -- they passed N
  if (is.null(data) | missing(data)) {

    # Single level -- maybe the user provided an ID_label, maybe they didn't.
    # Sanity check and/or construct an ID label for the new data.
    ID_label <- handle_id(ID_label, NULL)

    # Is the N argument passed here sane? Let's check
    N <- handle_n(N, add_level = TRUE)

    # Run the level adder, report the results, and return
    return(
      report_results(
        add_level_internal(
          N = N,
          ID_label = ID_label,
          working_environment_ = new_working_environment(),
          data_arguments = data_arguments,
          nest = TRUE
        )
      )
    )
  }

  working_environment <- import_data_list(data)

  # Single level -- maybe the user provided an ID_label, maybe they didn't.
  # Sanity check and/or construct an ID label for the new data.
  ID_label <- handle_id(ID_label, working_environment$data_frame_output_)

  # User passed data, not N
  # First, let's dynamically get N from the number of rows
  N <- nrow(working_environment$data_frame_output_)

  # Now, see if we need to staple one on
  if (ID_label %in% names(working_environment$data_frame_output_)) {
    add_level_id(working_environment, ID_label)
  } else if (length(data_arguments)) {
    working_environment$data_frame_output_[[ID_label]] <- generate_id_pad(N)
    add_level_id(working_environment, ID_label)
    add_variable_name(working_environment, ID_label)
  }

  # If the user does a passthrough for some reason, just return as is.
  if (!length(data_arguments)) {
    return(report_results(working_environment))
  }

  # Run the level adder, report the results, and return
  return(
    report_results(
      modify_level_internal(
        N = N,
        ID_label = ID_label,
        data_arguments = data_arguments,
        working_environment_ = working_environment
      )
    )
  )
}

#' Deprecated level call function maintained to provide useful error for
#' previous fabricatr code.
#' @keywords internal
#' @export
level <- function(N = NULL, ID_label = NULL, ...) {
  stop(
    "The `level()` function from early versions of fabricatr is deprecated. ",
    "Please use `add_level()` or `modify_level()` as necessary."
  )
  # Stub, this doesn't do anything yet -- may in the future dispatch to the
  # relevant levels.
}

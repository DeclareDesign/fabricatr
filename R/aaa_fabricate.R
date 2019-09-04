#' Fabricate data
#'
#' \code{fabricate} helps you simulate a dataset before you collect it. You can
#' either start with your own data and add simulated variables to it (by passing
#' \code{data} to \code{fabricate()}) or start from scratch by defining
#' \code{N}. Create hierarchical data with multiple levels of data such as
#' citizens within cities within states using \code{add_level()} or modify
#' existing hierarchical data using \code{modify_level()}. You can use any R
#' function to create each variable. Use \code{cross_levels()} and
#' \code{link_levels()} to make more complex designs such as panel or
#' cross-classified data.
#'
#' We also provide several built-in options to easily create variables, including
#' \code{\link{draw_binary}}, \code{\link{draw_count}}, \code{\link{draw_likert}},
#' and intra-cluster correlated variables \code{\link{draw_binary_icc}} and
#' \code{\link{draw_normal_icc}}
#'
#' @param data (optional) user-provided data that forms the basis of the
#' fabrication, e.g. you can add variables to existing data. Provide either
#' \code{N} or \code{data} (\code{N} is the number of rows of the data if
#' \code{data} is provided). If \code{data} and \code{N} are not provided,
#' fabricatr will try to interpret the first un-named argument as either \code{data}
#' or \code{N} based on type.
#' @param N (optional) number of units to draw. If provided as
#' \code{fabricate(N = 5)}, this determines the number of units in the
#' single-level data. If provided in \code{add_level}, e.g.
#' \code{fabricate(cities = add_level(N = 5))}, \code{N} determines the number
#' of units in a specific level of a hierarchical dataset.
#' @param ID_label (optional) variable name for ID variable, e.g. citizen_ID. Set to NA to suppress the creation of an ID variable.
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
#' head(multi_level_df)
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
#' fabricate(
#'   data = multi_level_df,
#'   regions = modify_level(watershed = sample(c(0, 1), N, replace = TRUE)),
#'   cities = modify_level(runoff = rnorm(N))
#' )
#'
#' # fabricatr can add variables that are higher-level summaries of lower-level
#' # variables via a split-modify-combine logic and the \code{by} argument
#'
#' multi_level_df <-
#'  fabricate(
#'    regions = add_level(N = 5, elevation = rnorm(N)),
#'    cities = add_level(N = 2, pollution = rnorm(N, mean = 5)),
#'    cities = modify_level(by = "regions", regional_pollution = mean(pollution))
#'  )
#'
#' # fabricatr can also make panel or cross-classified data. For more
#' # information about syntax for this functionality please read our vignette
#' # or check documentation for \code{link_levels}:
#' cross_classified <- fabricate(
#'   primary_schools = add_level(N = 50, ps_quality = runif(N, 0, 10)),
#'   secondary_schools = add_level(N = 100, ss_quality = runif(N, 0, 10), nest=FALSE),
#'   students = link_levels(N = 2000,
#'                           by=join_using(ps_quality, ss_quality, rho = 0.5),
#'                           student_quality = ps_quality + 3*ss_quality + rnorm(N)))
#' @seealso \code{\link{link_levels}}
#' @importFrom rlang quos quo_name eval_tidy call_name call_modify call_args call_args_names quo_squash
#' is_call get_expr
#'
#' @export
fabricate <- function(..., data = NULL, N = NULL, ID_label = NULL) {
  # Store all data generation arguments in a quosure for future evaluation
  # A quosure contains unevaluated formulae and function calls.
  dots <- quos(...)

  # Fabricatr expects either a single-level function call
  # or a series of level calls. You can't mix and match.
  # This helper function will be TRUE if calls are all levels, FALSE
  # if there are no calls or they are not levels.
  data_supplied <- !is.null(data)
  n_supplied    <- !is.null(N)
  all_levels    <- FALSE # recalculated after implicit N / data=
  explicit_ID_provided <- !missing(ID_label)


  # Maybe they anonymously passed data or N.
  if (!data_supplied && !n_supplied) {
    i <- which(names(dots) == "" & call_not_level_call(dots))[1]

    # i is na when dots is length zero, all names are provided => which is integer(0) and integer(0)[1] is NA
    if(!is.na(i)) {

      if (quo_squash(dots[[i]]) == "") {
        stop("There appears to be a blank argument. Is there a misplaced comma?", call. = FALSE)
      }

      first_unnamed_dot <- eval_tidy(dots[[i]])
      dots <- dots[-i]


      # If they supplied a list or data frame, they meant data.
      if(is.list(first_unnamed_dot)) {
        data <- first_unnamed_dot
        data_supplied <- TRUE
      }
      else if(is_scalar_integerish(first_unnamed_dot)) {
        N <- first_unnamed_dot
        n_supplied <- TRUE
      }
    }
  }

  # Now re-run the checks.
  all_levels <- check_all_levels(dots)

  if (n_supplied == (all_levels || data_supplied)) {
    fabricate_mode_error()
  }

  working_environment <- import_data_list(data)

  if (all_levels) {
    nm <- names(dots) %||% rep("", length(dots))
    # Ensure the user provided a name for each level call.
    # If they didn't, see if we can poach it out of the arguments
    for(i in seq_along(dots)) {
      if(nm[i] != "") next;

        # Can't salvage this one
        if(!"ID_label" %in% call_args_names(dots[[i]])) {
          stop("You must provide a name for each level that you create.")
        }

        names(dots)[i] <- call_args(dots[[i]])$ID_label
    }


    # Each of data_arguments is a level call
    for (i in seq_along(dots)) {
      # Add two variables to the argument of the current level call
      # one to pass the working environment so far
      # one to pass the ID_label the user intends for the level

      dots[[i]] <- call_modify(
        dots[[i]],
        working_environment_ = working_environment,
        ID_label = names(dots)[i]
      )

      # Execute the level build and pass it back to the current working
      # environment.
      working_environment <- eval_tidy(dots[[i]])
    }

    # Return the results from the working environment
    return(report_results(working_environment))
  }

  else if (n_supplied) {
    # Single level -- maybe the user provided an ID_label, maybe they didn't.
    # Sanity check and/or construct an ID label for the new data.
    ID_label <- handle_id(ID_label, NULL)

    # Is the N argument passed here sane? Let's check
    N <- handle_n(N, add_level = TRUE, working_environment)

    ret <- add_top_level_internal(
      N = N,
      ID_label = ID_label,
      workspace = working_environment,
      data_arguments = dots)
  }

  else if (data_supplied) {

    df <- active_df(working_environment)

    # Single level -- maybe the user provided an ID_label, maybe they didn't.
    # Sanity check and/or construct an ID label for the new data.
    ID_label <- handle_id(ID_label, df)

    # First, let's get N from the number of rows
    N <- nrow(df)

    # Now, see if we need to staple an ID column on. This is a bit of a mess.
    if(is.na(ID_label)) {

      # Explicit override of ID_label -- don't add

    } else if (ID_label %in% names(df)) {
      # There's already an ID column named the thing we want to call the ID
      # column, so keep it. If the user did not specify, then this shouldn't
      # happen because handle_id would have moved to a fallback ID.
    } else if(explicit_ID_provided) {
      # We explicitly asked for an ID column, so let's do it.
      df[[ID_label]] <- generate_id_pad(N)
    } else if(length(dots)) {
      # We didn't explicitly ask for an ID column, but we are modifying the data
      # so probably we should do it unless there's a column that's exactly this.
      # Generate the ID label and check if there's a column that's exactly this.
      temp_id <- generate_id_pad(N)

      id_matches <- vapply(lapply(df, as.character), identical, TRUE, temp_id)

      if(any(id_matches)) {
        ID_label <- names(df)[id_matches][1]
      } else {
        df[[ID_label]] <- temp_id
      }


    }

    uu <- attr(working_environment, "active_df")
    working_environment[[uu]] <- df

    # Run the level adder, report the results, and return
    ret <- if (is_empty(dots)) working_environment else
      modify_level_internal(
        N = N,
        ID_label = uu,
        data_arguments = dots,
        workspace = working_environment
      )
  }

  report_results(ret)
}



fabricate_mode_error <- function() {
  stop(
    "You must do exactly one of: \n",
    "1) One or more level calls, with or without existing data \n",
    "2) Import existing data and add new variables \n",
    "3) Provide an `N` without importing data or creating levels"
  )
}


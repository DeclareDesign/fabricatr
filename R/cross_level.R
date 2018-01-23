
#' Creates cross-classified (partially non-nested, joined data) with a fixed
#' correlation structure.
#'
#' @param N The number of observations in the resulting data frame.
#' If N is NULL or not provided, the join will be an "outer join" -- creating a
#' full panel of each of the rows from each data frame provided.
#' @param by The result of a call to \code{join()} which specifies how the
#' cross-classified data will be created
#' @param ... A variable or series of variables to add to the resulting data
#' frame after the cross-classified data is created.
#'
#' @return data.frame
#'
#' @examples
#'
#' # Generate full panel data
#'
#' panel <- fabricate(
#'  countries = add_level(N = 20, country_shock = runif(N, 1, 10)),
#'  years = add_level(N = 20, year_shock = runif(N, 1, 10), nest=FALSE),
#'  obs = cross_level(by=join(countries, years), GDP_it = country_shock + year_shock)
#' )
#'
#' # Generate cross-classified data and merge, no correlation
#' students <- fabricate(
#'  primary_school = add_level(N = 20, ps_quality = runif(N, 1, 10)),
#'  secondary_school = add_level(N = 15, ss_quality = runif(N, 1, 10), nest=FALSE),
#'  students = cross_level(N = 500, by = join(primary_school, secondary_school))
#' )
#' head(students)
#'
#' # Cross-classified data with a correlation structure
#' students <- fabricate(
#'  primary_school = add_level(N = 20, ps_quality = runif(N, 1, 10)),
#'  secondary_school = add_level(N = 15, ss_quality = runif(N, 1, 10), nest=FALSE),
#'  students = cross_level(N = 500, by = join(ps_quality, ss_quality, rho = 0.5))
#' )
#' cor(students$ps_quality, students$ss_quality)
#'
#' @importFrom rlang quos get_expr
#' @export
cross_level = function(N = NULL,
                       by = NULL,
                       ...) {

  data_arguments = quos(...)
  if("working_environment_" %in% names(data_arguments)) {
    working_environment_ = get_expr(data_arguments[["working_environment_"]])
    data_arguments[["working_environment_"]] = NULL
  } else {
    # This happens if either an add_level call is run external to a fabricate
    # call OR if add_level is the only argument to a fabricate call and
    # the data argument tries to resolve an add_level call.
    stop("`cross_level()` calls must be run inside `fabricate()` calls.")
  }
  if("ID_label" %in% names(data_arguments)) {
    ID_label = get_expr(data_arguments[["ID_label"]])
    data_arguments[["ID_label"]] = NULL
  }

  return(cross_level_internal(N = N, ID_label = ID_label, by = by,
                              working_environment_ = working_environment_,
                              data_arguments = data_arguments))
}

#' @importFrom rlang quo_text eval_tidy
cross_level_internal = function(N = NULL,
                                ID_label = NULL,
                                working_environment_ = NULL,
                                by = NULL,
                                data_arguments = NULL) {

  if(any(!c("data_frame_output_", "shelved_df") %in%
         names(working_environment_))) {
    stop("You must provide at least two separate level hierarchies to create ",
         "cross-classified data. If you have specified multiple levels, please ",
         "ensure that you use the `nest=FALSE` argument to specify they are ",
         "non-nested")
  }

  if(is.null(by) || !length(by$variable_names)) {
    stop("You must specify a join structure using the `by` argument to create ",
         "cross-classified data.")
  }

  # Shelf the working data frame before continuing, so now all our data is on
  # the shelf.
  working_environment_ = shelf_working_data(working_environment_)

  # Loop over the variable name
  variable_names = by$variable_names
  data_frame_indices = integer(length(variable_names))

  if(anyDuplicated(variable_names)) {
    stop("Variable names for joining cross-classified data must be unique. ",
         "Currently, you are joining on a variable named \"",
         variable_names[anyDuplicated(variable_names)[1]],
         "\" more than once.")
  }

  # Figure out which dfs we're joining on which variables
  for(i in seq_along(variable_names)) {
    for(j in seq_along(working_environment_$shelved_df)) {
      if(variable_names[i] %in%
         working_environment_$shelved_df[[j]]$variable_names_) {

        # If we've already found this one, that's bad news for us...
        if(data_frame_indices[i]) {
          stop("The variable name ",
               variable_names[i],
               " is ambiguous and appears in at least two level hierarchies. ",
               "All level names used for joining must be unique.")
        }

        data_frame_indices[i] = j
      }
    }

    # If we didn't find this one, that's bad news for us...
    if(!data_frame_indices[i]) {
      stop("The variable name ",
           variable_names[i],
           " that you specified as part of your `cross_level()` join was not ",
           "found in any of the level hierarchies")
    }
  }

  if(anyDuplicated(data_frame_indices)) {
    stop("You can't join a level hierarchy to itself.")
  }

  # Actually fetch the df objects
  data_frame_objects = sapply(data_frame_indices,
                              function(x) {
                                working_environment_$shelved_df[[x]]$data_frame_output_
                              },
                              simplify = FALSE
  )

  if(is.null(N) && (!is.null(by$sigma) || by$rho)) {
    stop("When `N` is null in a `cross_level()` call, the data generated is a ",
         "complete panel of all observations in each data frame and cannot have ",
         "a specified correlation structure. Please remove the correlation structure ",
         "from the `by` argument.")
  }

  # Do the join.
  if(!is.null(N)) {
    out = join_dfs(data_frame_objects, variable_names, N, by$sigma, by$rho)
  } else {
    out = panel_dfs(data_frame_objects)
    N = nrow(out)
  }
  working_environment_$variable_names_ = names(out)

  # Staple in an ID column onto the data list.
  if(!is.null(ID_label) && (!ID_label %in% names(out))) {
    out[, ID_label] = generate_id_pad(N)

    add_level_id(working_environment_, ID_label)
    add_variable_name(working_environment_, ID_label)
  }

  # Overwrite the working data frame.
  working_environment_$data_frame_output_ = out

  if(length(data_arguments)) {
    working_environment_ = modify_level_internal(ID_label = ID_label,
                                                 working_environment_ = working_environment_,
                                                 data_arguments = data_arguments)
  }

  # Return results
  return(working_environment_)
}

#' Helper function handling specification of which variables to join a
#' cross-classified data on, and what kind of correlation structure needed
#' @param ... A series of two or more variable names, unquoted, to join on in
#' order to create cross-classified data.
#' @param rho A fixed (Spearman's rank) correlation coefficient between the
#' variables being joined on: note that if it is not possible to make a
#' correlation matrix from this coefficient (e.g. if you are joining on three
#' or more variables and rho is negative) then the \code{cross_level()} call
#' will fail.
#' @param sigma A matrix with dimensions equal to the number of variables you
#' are joining on, specifying the correlation for the resulting joined data.
#' Only one of rho and sigma should be provided.
#' @export
join = function(..., rho=0, sigma=NULL) {
  data_arguments = quos(...)
  variable_names = unlist(lapply(data_arguments, function(x) { quo_text(x) }))
  return(list(variable_names = variable_names,
              rho = rho,
              sigma = sigma))
}

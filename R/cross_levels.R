
#' Creates panel or cross-classified data
#'
#' This function allows the user to create data structures that are paneled or
#' cross-classified: where one level of observation draws simultaneously from
#' two or many source levels. Common examples of panels include country-year
#' data which have country-level and year-level characteristics.
#'
#' By specifying the appropriate arguments in \code{join_using()} within the
#' function call, it is possible to induce correlation in cross-classified data.
#'
#' @param N The number of observations in the resulting data frame.
#' If \code{N} is NULL or not provided, the join_using will be an "outer product" --
#' merging each row of each provided data frame with each other data frame to
#' make a full panel.
#' @param by The result of a call to \code{join_using()} which specifies how
#' the cross-classified data will be created
#' @param ... A variable or series of variables to add to the resulting data
#' frame after the cross-classified data is created.
#'
#' @return data.frame
#'
#' @examples
#'
#' # Generate full panel data
#' panel <- fabricate(
#'  countries = add_level(N = 20, country_shock = runif(N, 1, 10)),
#'  years = add_level(N = 20, year_shock = runif(N, 1, 10), nest=FALSE),
#'  obs = cross_levels(by = join_using(countries, years), GDP_it = country_shock + year_shock)
#' )
#'
#' # Include an "N" argument to allow for cross-classified
#' # data.
#' students <- fabricate(
#'  primary_school = add_level(N = 20, ps_quality = runif(N, 1, 10)),
#'  secondary_school = add_level(N = 15, ss_quality = runif(N, 1, 10), nest=FALSE),
#'  students = link_levels(N = 500, by = join_using(primary_school, secondary_school))
#' )
#' head(students)
#'
#' # Induce a correlation structure in cross-classified data by providing
#' # rho.
#' students <- fabricate(
#'  primary_school = add_level(N = 20, ps_quality = runif(N, 1, 10)),
#'  secondary_school = add_level(N = 15, ss_quality = runif(N, 1, 10), nest=FALSE),
#'  students = link_levels(N = 500, by = join_using(ps_quality, ss_quality, rho = 0.5))
#' )
#' cor(students$ps_quality, students$ss_quality)
#'
#' @importFrom rlang quos
#' @export
cross_levels <- function(by = NULL,
                         ...) {
  data_arguments <- quos(...)
  if ("N" %in% names(data_arguments) ||
    !is.null(by$sigma) || by$rho) {
    stop(
      "`cross_levels()` calls are used to create full panels and cannot take ",
      "`N` arguments or correlation structures."
    )
  }

  do_internal(N = NULL, by = by, ..., FUN=cross_levels_internal, from="cross_level" )
}

#' @importFrom rlang quos get_expr
#'
#' @rdname cross_levels
#' @export
link_levels <- function(N = NULL, by = NULL, ...) {
  do_internal(N, by = by, ..., FUN = cross_levels_internal, from="link_level")
}





#' @importFrom rlang quo_text eval_tidy
cross_levels_internal <- function(N = NULL,
                                  ID_label = NULL,
                                  workspace = NULL,
                                  by = NULL,
                                  data_arguments = NULL) {

  check_cross_level_args(workspace, by)



  variable_names <- by$variable_names

  df_names <- names(workspace)
  data_frame_indices <- integer(length(variable_names))

  # Figure out which dfs we're joining on which variables
  for (i in seq_along(variable_names)) {
    for (j in seq_along(df_names)) {
      if (variable_names[i] %in% names(workspace[[df_names[j]]])) {

        # If we've already found this one, that's bad news for us...
        if (data_frame_indices[i]) {
          stop(
            "The variable name ",
            variable_names[i],
            " is ambiguous and appears in at least two level hierarchies. ",
            "All level names used for joining must be unique."
          )
        }

        data_frame_indices[i] <- j
      }
    }

    # If we didn't find this one, that's bad news for us...
    if (!data_frame_indices[i]) {
      stop(
        "The variable name ",
        variable_names[i],
        " that you specified as part of your `cross_levels()` join was not ",
        "found in any of the level hierarchies"
      )
    }
  }

  if (anyDuplicated(data_frame_indices)) {
    stop("You can't join a level hierarchy to itself.")
  }

  # Actually fetch the df objects
  data_frame_objects <- mget(df_names[data_frame_indices], workspace)


  # Do the join.
  if (!is.null(N)) {
    out <- join_dfs(data_frame_objects, variable_names, N, by$sigma, by$rho)
  } else {
    out <- panel_dfs(data_frame_objects)
    N <- nrow(out)
  }

  # Staple in an ID column onto the data list.
  if (!is.null(ID_label) && (!ID_label %in% names(out))) {
    out[, ID_label] <- generate_id_pad(N)

  }

  append_child(workspace, ID_label, df_names[data_frame_indices], out)

  activate(workspace, ID_label);

  if (length(data_arguments)) {
    workspace <- modify_level_internal(
      ID_label = ID_label,
      workspace = workspace,
      data_arguments = data_arguments
    )
  }

  # Return results
  workspace
}

#' Helper function handling specification of which variables to join a
#' cross-classified data on, and what kind of correlation structure needed.
#' Correlation structures can only be provided if the underlying call is
#' a \code{link_levels()} call.
#'
#' @param ... A series of two or more variable names, unquoted, to join on in
#' order to create cross-classified data.
#' @param rho A fixed (Spearman's rank) correlation coefficient between the
#' variables being joined on: note that if it is not possible to make a
#' correlation matrix from this coefficient (e.g. if you are joining on three
#' or more variables and rho is negative) then the \code{cross_levels()} call
#' will fail. Do not provide \code{rho} if making panel data.
#' @param sigma A matrix with dimensions equal to the number of variables you
#' are joining on, specifying the correlation for the resulting joined data.
#' Only one of rho and sigma should be provided. Do not provide \code{sigma} if
#' making panel data.
#' @examples
#'
#' panels <- fabricate(
#'   countries = add_level(N = 150, country_fe = runif(N, 1, 10)),
#'   years = add_level(N = 25, year_shock = runif(N, 1, 10), nest = FALSE),
#'   obs = cross_levels(
#'     by = join_using(countries, years),
#'     new_variable = country_fe + year_shock + rnorm(N, 0, 2)
#'   )
#' )
#'
#' schools_data <- fabricate(
#'   primary_schools = add_level(N = 20, ps_quality = runif(N, 1, 10)),
#'   secondary_schools = add_level(
#'     N = 15,
#'     ss_quality = runif(N, 1, 10),
#'     nest = FALSE),
#'   students = link_levels(
#'     N = 1500,
#'     by = join_using(primary_schools, secondary_schools),
#'     SAT_score = 800 + 13 * ps_quality + 26 * ss_quality + rnorm(N, 0, 50)
#'   )
#' )
#'
#' @export
join_using <- function(..., rho=0, sigma=NULL) {
  data_arguments <- quos(...)
  variable_names <- unlist(lapply(data_arguments,quo_text))
  list(
    variable_names = variable_names,
    rho = rho,
    sigma = sigma
  )
}


check_cross_level_args <- function(workspace, by) {
  if(length(workspace) <= 1){
    stop(
      "You must provide at least two separate level hierarchies to create ",
      "cross-classified data. If you have specified multiple levels, please ",
      "ensure that you use the `nest=FALSE` argument to specify they are ",
      "non-nested"
    )
  }

  if (is.null(by) || length(by$variable_names) == 0) {
    stop(
      "You must specify a join structure using the `by` argument to create ",
      "cross-classified data."
    )
  }


  if (anyDuplicated(by$variable_names)) {
    stop(
      "Variable names for joining cross-classified data must be unique. ",
      "Currently, you are joining on a variable named \"",
      unique(by$variable_names[duplicated(by$variable_names)]),
      "\" more than once."
    )
  }
}

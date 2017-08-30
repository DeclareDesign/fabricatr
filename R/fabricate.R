



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
fabricate <-
  function(data = NULL,
           N = NULL,
           ID_label = NULL,
           ...) {
    options <- quos(...)

    functions_or_not <-
      sapply(options, function(i) {
        is_lang(get_expr(i))
      })

    if (length(functions_or_not) > 0) {
      options_fn <-
        sapply(options[functions_or_not], lang_name) ## function names
      if (any(options_fn == "level") &
          !all(options_fn == "level")) {
        stop(
          "Arguments passed to ... must either all be calls to level() or have no calls to level()."
        )
      }
      all_levels <-
        all(options_fn == "level") &
        length(options_fn) > 0 & all(functions_or_not)
    } else{
      all_levels <- FALSE
    }

    if (!is.null(data) & !any(class(data) == "data.frame")) {
      stop(
        "Please provide a data object to the data argument, e.g. a data.frame, tibble, or sf object."
      )
    }

    ID_label <- substitute(ID_label)
    if (!is.null(ID_label)) {
      ID_label <- as.character(ID_label)
    }

    # check if all the options are level calls
    if (all_levels) {
      for (i in seq_along(options)) {
        # Pop the data from the previous level in the current call
        # Do this if there existing data to start with or
        #   and beginning with the second level
        if (i > 1 | !is.null(data)) {
          options[[i]] <- lang_modify(options[[i]], data_internal_ = data)
        }

        # Also do a sweet switcheroo with the level names
        options[[i]] <-
          lang_modify(options[[i]], ID_label_ = names(options)[i])

        # update the current data
        data <- eval_tidy(options[[i]])

      }

      return(data)

    } else {
      fabricate_data_single_level(
        data = data,
        N = N,
        ID_label = ID_label,
        existing_ID = !is.null(data) & is.null(ID_label),
        ... = ...
      )
    }
  }


#' @importFrom rlang quos eval_tidy
fabricate_data_single_level <- function(data = NULL,
                                        N = NULL,
                                        ID_label = NULL,
                                        ...,
                                        existing_ID = FALSE) {
  if (sum(!is.null(data),!is.null(N)) != 1) {
    stop("Please supply either a data.frame or N and not both.")
  }

  if (is.null(data)) {
    if (length(N) > 1) {
      stop(paste0(
        "At the top level, ",
        ID_label,
        ", you must provide a single number to N."
      ))
    }
    # make IDs that are nicely padded
    data <-
      data.frame(sprintf(paste0("%0", nchar(N), "d"), 1:N), stringsAsFactors = FALSE)

    # this creates column names from ID_label
    # note if ID_label is NULL that the ID column name is just "ID" -- so safe
    colnames(data) <- ifelse(is.null(ID_label), "ID", ID_label)
  } else {
    N <- nrow(data)
    if (existing_ID == FALSE) {
      data[, ifelse(is.null(ID_label), "ID", ID_label)] <-
        sprintf(paste0("%0", nchar(nrow(data)), "d"), 1:nrow(data))
    }
  }

  args <- quos(...)

  args_names <- names(args)

  if (length(args) > 0) {
    for (i in 1:length(args)) {
      # this was changed to move costly data.frame operations inside the loop
      #   because previously, if you did rnorm(N) in a level
      #   it literally did a vector of length N, rather than doing
      #   it for all of the values of the level above it, i.e. if this
      #   this is the second level and N = 2, it made a vector of length 2
      #   even though there were 5 units in the higher level so there should
      #   have been a vector of 5*2 = 10
      # NB: this is still not super safe; if the expression returns a thing
      # of length not exactly to N it's just going to repeat it as it does
      # data.frame(data_list). Usually this shouldn't be a problem but we may
      # want a warning or error
      data_list <- as.list(data)
      data_list$N <- N
      data_list[[args_names[i]]] <-
        eval_tidy(args[[i]], data_list)
      data_list$N <- NULL
      data <- data.frame(data_list, stringsAsFactors = FALSE)
    }
  }

  rownames(data) <- NULL
  return(data)
}

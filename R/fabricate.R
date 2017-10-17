



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

    # Helper expression to check if provided options are R functions
    functions_or_not <-
      sapply(options, function(i) {
        is_lang(get_expr(i))
      })

    # If it's a multi-level dataset, every argument must be a level
    if (length(functions_or_not) > 0) {
      options_fn <-
        sapply(options[functions_or_not], lang_name) ## function names
      if (any(options_fn == "level") &
          !all(options_fn == "level")) {
        stop(
          "Arguments passed to ... must either all be calls to level() or have no calls to level()."
        )
      }
      # Cache result: all arguments are levels
      all_levels <-
        all(options_fn == "level") &
        length(options_fn) > 0 & all(functions_or_not)
    } else{
      # Cache result: all arguments are not levels
      all_levels <- FALSE
    }

    # Processing user-provided data frame
    if (!any(is.null(data)) & !any(class(data) == "data.frame")) {
      # Not a data frame, but we might be able to make it one
      if(any(class(data) == "matrix")) {
        conversion_result = tryCatch({
          data <- as.data.frame(data)
        }, error=function(e) {
          stop(
            "Provided data object was a matrix and could not be converted to a data.frame. Please provide a data.frame, tibble, or sf object."
          )
        })
      } else {
        if(!"data" %in% names(sys.call())) {
          stop(
            "The data argument must be a data object. The argument call, ", deparse(substitute(data)), ", was not a data object (e.g. a data.frame, tibble, sf object, or convertible matrix)."
          )
        } else {
          # Not a data frame, need to error out.
          stop(
            "Please provide a data object to the data argument, e.g. a data.frame, tibble, convertible matrix, or sf object."
          )
        }
      }
    }

    # ID label is a non-standard evaluated variable name
    if(is.symbol(substitute(ID_label))) {
      ID_label <- substitute(ID_label)
      # Ensure it's not null, in case substitution still passed a null
      if(!is.null(ID_label)) {
        ID_label <- as.character(ID_label)
      }
    } else if(!is.null(ID_label)) {
      if(is.vector(ID_label) & length(ID_label) > 1) {
        # Vector of length n>1, error
        stop("Provided ID_label must be a character vector of length 1 or variable name.")
      } else if(is.vector(ID_label) & is.numeric(ID_label[1])) {
        # Numeric ID_label
        warning("Provided ID_label is numeric and will be prefixed with the character \"X\"")
        ID_label <- as.character(ID_label)
      } else if(is.vector(ID_label) & is.character(ID_label[1])) {
        # Valid ID_label
        ID_label <- as.character(ID_label)
      } else if(!is.null(dim(ID_label))) {
        stop("Provided ID_label must be a character vector or variable name, not a data frame or matrix.")
      }
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

  # This should have been taken care of by fabricate, but let's ensure at this level as well
  if(is.null(data) & is.null(N)) {
    stop("You must provide either the data argument or the N argument to fabricate single-level data.")
  }
  if (sum(!is.null(data),!is.null(N)) > 1) {
    stop("You must provide either the data argument or the N argument, but not both. Currently, data=", deparse(data), " and N=", deparse(N))
  }

  # If we provided an N, it must be sane
  if (is.null(data)) {
    # No N provided
    if (is.null(N)) {
      stop(
        "You must provide either data or N to each level of data."
      )
    }
    # N is not numeric or else it's a non-integer number.
    if(length(N) == 1 & is.numeric(N) & any(!N%%1 == 0)) {
      stop(paste0(
        "The provided N must be an integer number. Provided N was of type ",
        typeof(N)
      ))
    }
    # If N is a vector of integers, that's bad too.
    if (length(N) > 1) {
      # Make sure the error message shows the ID_label if there is one.
      if (is.null(ID_label)) {
        stop(
          "At the top level, you must provide a single number to N"
        )
      } else {
        stop(paste0(
          "At the top level, ",
          ID_label,
          ", you must provide a single number to N."
        ))
      }
    }

    if (N <= 0) {
      stop(
        "N must be a positive integer."
      )
    }

    # make IDs that are nicely padded
    data <-
      data.frame(sprintf(paste0("%0", nchar(N), "d"), 1:N), stringsAsFactors = FALSE)

    # this creates column names from ID_label
    # note if ID_label is NULL that the ID column name is just "ID" -- so safe
    colnames(data) <- ifelse(is.null(ID_label), "ID", ID_label)
  } else {
    # User is providing data, but we still want to staple an ID column in if necessary.
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





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
  function(data,
           N,
           ID_label,
           ...) {

    # Each level argument passed to fabricate will be stored in options
    # as an unevaluated language call.
    options <- quos(...)

    # Let's check if we have nothing but level calls.
    all_levels <- check_all_levels(options)

    # We've got data, but it's not a data frame
    if (!missing(data) && !"data.frame"  %in% class(data)) {
      # It's not at least 2d
      if(is.null(dim(data))) {
          stop(
            "User provided data must be a data frame. Provided data was low dimensional."
          )
      }
      # We got something that thinks it is data, but it wasn't explicitly given as data
      if(!"data" %in% names(sys.call())) {
        stop(
          "The data argument must be a data object. The argument call, ", deparse(substitute(data)), ", was not a data object (e.g. a data.frame, tibble, sf object, or convertible matrix)."
        )
      }
      # Let's see if we can make it a data frame
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

    # They were all level calls, so we need to build the level one by one
    if (all_levels) {
      for (i in seq_along(options)) {
        # If we have been passed data or if we are in at least the second level
        # pass the existing data frame to the variable data_internal_
        # as an argument to the level generation call.
        if (i > 1 | !missing(data)) {
          options[[i]] <- lang_modify(options[[i]], data_internal_ = data)
        }

        # Adds the variable ID_label_ to the quosure at the current level
        # equal to the value of names(options)[i] -- the "variable" we're assigning
        # the level to
        options[[i]] <-
          lang_modify(options[[i]], ID_label_ = names(options)[i])

        # Execute the current level call in the context of all the data it can see.
        data <- eval_tidy(options[[i]])
      }

      # Return the final, assembled data.
      return(data)

    } else {
      # No level calls, this is single-level data

      # No user provided data
      if(missing(data)) data <- NULL
      # No user provided N
      if(missing(N)) N <- NULL
      # No user provided ID_label
      if(missing(ID_label)) ID_label <- NULL

      # Building ID label from what the user provided
      # It's a language symbol -- if so, use the symbol, not the value
      if(is.symbol(substitute(ID_label))) {
        ID_label <- substitute(ID_label)
        if (!is.null(ID_label)) {
          ID_label <- as.character(ID_label)
        }
      } else if(!is.null(ID_label)) {
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

      # We have our data, N, ID_label, whether there's an ID label, and pass through any other quosure stuff
      fabricate_data_single_level(
        data = data,
        N = N,
        ID_label = ID_label,
        existing_ID = !is.null(data) & is.null(ID_label),
        options = options
      )
    }
  }


#' @importFrom rlang quos eval_tidy
fabricate_data_single_level <- function(data = NULL,
                                        N = NULL,
                                        ID_label = NULL,
                                        ...,
                                        existing_ID = FALSE,
                                        options=quos(...)) {
  # The user provided nothing of what we want.
  if (is.null(data) == is.null(N)) {
    stop("Please supply either a data.frame or N and not both.")
  }

  # They provided an N
  if (!is.null(N)) {
    # The N is not a single integer but we're at the top level
    # We know we're at the top level because fabricate_data_single_level is only called if we are
    if (length(N) != 1) {
      # Error message explaining to the user where they messed up
      stop(
        "At the top level, ",
        ifelse(!is.null(ID_label),
               paste0(ID_label, ", "),
               ""),
        "you must provide a single number to N"
      )
    } else if(is.numeric(N) & any(!N%%1 == 0 | N<=0)) {
      # N has to be an integer above 0
      stop(
        "The provided N must be an integer number greater than 0. Provided N was of type ",
        typeof(N)
      )
    }

    # N is not numeric
    if(!is.numeric(N)) {
      tryCatch({
        # Let's try to force it to be numeric.
        N = as.numeric(N)
      }, error=function(e) {
        stop(
          "The provided value for N must be an integer number."
        )
      })
    }

    # Set up a data frame that's blank
    data <- data.frame()
    # There's no existing ID column because we're generating all the data.
    existing_ID <- FALSE
  } else if(!is.null(data)){
    # The user gave us data, so we have an N, it's the number of rows
    N <- nrow(data)
  }

  # If there's no ID column, we'll generate one now
  if (!existing_ID) {
    # If they didn't specify an ID label for the ID column, we just give it the name ID
    if(is.null(ID_label)) ID_label <- "ID"
    data <- genID(data, ID_label, N)
  }

  # Let's fab the data.
  fab(data, options)
}

genID <- function(data, ID, N=nrow(data)){
  # Left-Pad ID variable with zeroes
  fmt <- paste0("%0", nchar(N), "d")
  # Add it to the data frame.
  data[1:N, ID] <- sprintf(fmt, 1:N)
  data
}

fab <- function(data, args) {
  # This was explicitly provided above but ends up getting to be implicit because we
  # created the ID column with nrow(data) N.
  N <- nrow(data)

  # Convert the provisional DF to a list so that we can access it in the environment
  # of running the formulae for the next variable
  data_list = as.list(data)

  if(is.null(names(args)) || any(names(args) == "")) {
    stop("All variables specified at this level should have names.")
  }

  # Apparently we allow overwriting the data?
  #if(any(duplicated(names(args)))) {
  #  stop("All variables specified at this level should be unique.")
  #}

  print("Begin fab")
  print(args)

  for (i in names(args)) {
    # Debug to get a grip on what's going on here
    print(paste0("Generating data named ", i))
    print(args[[i]])

    # i is the variable name
    # args[[i]] is the formula for this variable
    # data_list contains the current working environment

    # Add a variable called N so that things have access; then evaluate the current
    # formula, adding it to the environment. Store it in the working data frame
    data_list[[i]] <-
      eval_tidy(args[[i]], append(data_list, list(N=N)))

    # If we have two arguments with exactly the same name, this will nuke the first
    # in the list, allowing the next one to be accessible the next time we get to it.
    args[[i]] <- NULL
  }

  data <- data.frame(data_list, stringsAsFactors = FALSE, row.names=NULL)

  # Return to the complete data frame
  return(data)
}


check_all_levels <- function(options){
  print(options)

  # Passing the options quosures
  # There were no levels at all
  if (length(options) == 0)  return(FALSE)

  # get_expr returns the expression for an item in a quosure
  # is_lang checks if it's a function
  is_function <- sapply(options, function(i) {
      is_lang(get_expr(i))
  })

  # lang_name gets function name from a quosure
  # compare this to level to see if it's a level
  is_level <- "level" == sapply(options[is_function], lang_name) ## function names

  # Return false if we have no level calls
  if(length(is_level) == 0) return(FALSE)

  # If some calls are levels and some aren't, we're unhappy
  if (any(is_level) != all(is_level)) {
    stop(
      "Arguments passed to ... must either all be calls to level() or have no calls to level()."
    )
  }

  # Confirm they're all levels
  is_level[1] && length(is_level) == length(options)
}


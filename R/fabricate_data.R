
#' Fabricate data
#'
#' @param ... Data generating arguments, such as \code{my_var = rnorm(N)}. You may also pass \code{level()} arguments, which define a level of a multi-level dataset. For example, you could send to \code{...} \code{level(my_level, var = rnorm)}. See examples.
#'
#' @param N number of units to draw
#' @param ID_label variable name for ID variable, i.e. citizen_ID (optional)
#' @param data user-provided data that forms the basis of the fabrication, i.e. you can add variables to existing data. Provide either \code{N} or \code{data} (\code{N} is the number of rows of the data if \code{data} is provided).
#'
#' @return data.frame
#'
#' @examples
#'
#' # Draw a single-level dataset with no covariates
#' df <- fabricate_data(N = 100)
#' head(df)
#'
#' # Draw a single-level dataset with a covariate
#' df <- fabricate_data(
#'   N = 100,
#'   height_ft = runif(N, 3.5, 8)
#' )
#' head(df)
#'
#' # Draw a two-level hierarchical dataset
#' # containing cities within regions
#' df <- fabricate_data(
#'  regions = level(N = 5),
#'  cities = level(N = 10, pollution = rnorm(N, mean = 5)))
#' head(df)
#'
#' @importFrom lazyeval lazy_dots dots_capture lazy_eval
#'
#' @export
fabricate_data <- function(..., N = NULL, ID_label = NULL, data = NULL) {

  options <- lazy_dots(...)
  options_text <- as.character(eval(substitute(alist(...))))

  # check if all the options are level calls
  if (all(sapply(options_text, function(x)
    startsWith(x, "level("))) &
    length(options_text) > 0) {

    if (!is.null(data)) {

      ##stop("If you are using levels, please don't include data as an argument; instead, use level_data within the levels argument, i.e. level(level_data = your_data).")
      ## instead, just let it start with the existing data
    }

    ## make this work! needs to do a loop through all the options

    # iff there are multiple levels, please to continue
    if ((length(options) + !is.null(data)) >= 1) {

      for (i in seq_along(options)) {
        # Pop the data from the previous level in the current call
        # Do this if there existing data to start with or
        #   and beginning with the second level
        if (i > 1 | !is.null(data)) {
          options[[i]]$expr$data <- data
        }

        # Also do a sweet switcheroo with the level names if applicable.
        if (is.null(options[[i]]$expr$ID_label)) {
          options[[i]]$expr$ID_label <- names(options)[i]
        }
        # update the current data
        data <- lazy_eval(options[[i]])

      }
    }

    return(data)

  } else {
    # Sometimes life is simple
    fabricate_data_single_level_(data = data, N = N, ID_label = ID_label, dots_capture(...))
  }
}


#' @importFrom lazyeval f_eval as_f_list
fabricate_data_single_level_ <- function(
  data = NULL, N = NULL, ID_label = NULL, args, existing_ID = FALSE) {
  if (sum(!is.null(data),!is.null(N)) != 1) {
    stop("Please supply either a data.frame or N and not both.")
  }

  if (is.null(data)) {

    if (length(N) > 1) {
      stop(paste0("At the top level, ", ID_label, ", you must provide a single number to N."))
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

  args <- as_f_list(args)

  for (nm in names(args)) {
    # inspired by lazyeval vignette
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
    data_list[[nm]] <- f_eval(args[[nm]], data_list)
    data_list$N <- NULL
    data <- data.frame(data_list, stringsAsFactors = FALSE)
  }

  rownames(data) <- NULL
  return(data)
}


#' @importFrom lazyeval dots_capture
fabricate_data_single_level <- function(data = NULL, N = NULL, ID_label = NULL, ...) {
  fabricate_data_single_level_(data = data, N = N, ID_label = ID_label, dots_capture(...))
}

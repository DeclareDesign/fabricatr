
#' Fabricate a Level of Data for Multi-Level Hierarchical Data
#'
#' @param ID_label variable name for ID variable, i.e. citizen_ID (optional)
#'
#' @param N number of units to draw in the level
#' @param ... Data generating arguments, such as \code{my_var = rnorm(N)}. You may also pass \code{level()} arguments, which define a level of a multi-level dataset. For example, you could send to \code{...} \code{level(my_level, var = rnorm)}. See examples.
#' @param level_data user-provided data that forms the basis of the fabrication at this level, i.e. you can add variables to existing data. Provide either \code{N} or \code{data} (\code{N} is the number of rows of the data if \code{data} is provided).
#' @param by name of variable to merge by to level above, if the level is not at the top of the hierarchy.
#' @param data system option that is used by fabricate_data to send data from upper levels of hierarchy to this level. Do not provide data.
#'
#' @examples
#'
#' # Draw a two-level hierarchical dataset
#' # containing cities within regions
#' df <- fabricate_data(
#'  regions = level(N = 5),
#'  cities = level(N = 10, pollution = rnorm(N, mean = 5)))
#' head(df)
#'
#' # Use existing data in a level using level_data
#' region_data <- data.frame(capital = c(1, 0, 0, 0, 0))
#' fabricate_data(regions = level(level_data = region_data,
#'                                gdp = runif(5)),
#'                cities = level(N = 5,
#'                               subways = rnorm(N, mean = 5)))
#'
#' @export
level <- function(ID_label, N = NULL, ..., level_data = NULL, by = NULL, data = NULL){

  ## level_data is existing data to begin this level with
  ## data is data from the level above this

  by <- substitute(by)
  if (!is.null(by)) {
    by <- as.character(by)
  }

  ID_label <- substitute(ID_label)
  if (!is.null(ID_label)) {
    ID_label <- as.character(ID_label)
  }

  if (is.null(data) & is.null(level_data)) {

    if (is.null(N)) {
      stop(paste0("If you do not provide data to level", ID_label, ", please provide N."))
    }
    if (length(N) > 1) {
      stop(paste0("At the top level, ", ID_label, ", you must provide a single number to N."))
    }
    # make IDs that are nicely padded
    data <- data.frame(sprintf(paste0("%0", nchar(N), "d"), 1:N), stringsAsFactors = FALSE)
    colnames(data) <- ID_label

  } else {

    if (is.null(level_data)) {

      # either provide an existing ID_label or N if you don't provide custom data

      # if there is no ID variable, expand the dataset based on the commands in N
      if (!ID_label %in% colnames(data)) {

        # this check copied and pasted from purrr
        N <- eval(substitute(N), envir = data)
        if (typeof(N) %in% c("integer", "double") && length(N) == 1) {
          data <- data[rep(1:nrow(data), each = N), , drop = FALSE]
        } else if (typeof(N) %in% c("integer", "double") && length(N) > 1) {
          # check that the vector that is N is the right length, i.e the length of data
          if (length(N) != nrow(data)) {
            stop(paste0(
              "If you provide a vector to N for level", ID_label,
              ", it must be the length of the dataset at the level above it ",
              "in the hierarchy"))
          }
          data <- data[rep(1:nrow(data), times = N), , drop = FALSE]
        } else if (class(N) == "function") {
          data <- data[rep(1:nrow(data), times = N()), , drop = FALSE]
        } else {
          stop(paste0(
            "Please provide level ", ID_label,
            " with N that is a vector, scalar, or function that generates a vector."))
        }
      } else {
        # otherwise assume you are adding variables to an existing level
        # defined by the level ID variable that exists in the data

        ## identify variables that do not vary within ID_label
        ## maybe there is a faster way to do this?
        level_variables <- sapply(colnames(data)[!colnames(data) %in% ID_label], function(i)
          max(tapply(data[, i], list(data[,ID_label]),
                     function(x) length(unique(x)))) == 1)
        level_variables <- names(level_variables)[level_variables]

        level_data <- unique(data[, unique(c(ID_label, level_variables)), drop = FALSE])

        level_data <- fabricate_data_single_level_(
          data = level_data, N = NULL, ID_label = ID_label,
          args = dots_capture(...), existing_ID = TRUE)

        return(merge(data[, colnames(data)[!(colnames(data) %in%
                                               level_variables)], drop = FALSE],
                     level_data,
                     by = as.character(substitute(ID_label)), all = TRUE, sort = FALSE))

      }

    } else {
      ## if they sent level_data start with that

      if (!is.null(N)) {
        stop(paste0("Please provide level ", ID_label, " with either level_data or N, not both."))
      }

      if (!is.null(data)) {
        data <- merge(data, level_data, by = by, all = TRUE, sort = FALSE)
      } else {
        data <- level_data
      }

    }

  }

  # now that data is the right size, pass to "mutate", i.e., simulate data

  fabricate_data_single_level_(data = data, N = NULL,
                               ID_label = ID_label, dots_capture(...))
}





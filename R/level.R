

#' Fabricate a Level of Data for Multi-Level Hierarchical Data
#'
#' @param ID_label variable name for ID variable, i.e. citizen_ID (optional)
#'
#' @param N number of units to draw in the level
#' @param ... Data generating arguments, such as \code{my_var = rnorm(N)}. You may also pass \code{level()} arguments, which define a level of a multi-level dataset. For example, you could send to \code{...} \code{level(my_level, var = rnorm)}. See examples.
#' @param data user-provided data that forms the basis of the fabrication at this level, i.e. you can add variables to existing data. Provide either \code{N} or \code{data} (\code{N} is the number of rows of the data if \code{data} is provided).
#' @param by name of variable to merge by to level above, if the level is not at the top of the hierarchy.
#'
#' @importFrom rlang quos eval_tidy quo lang_modify
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
#' # Use existing data in a level using data
#' region_data <- data.frame(capital = c(1, 0, 0, 0, 0))
#' fabricate_data(regions = level(data = region_data,
#'                                gdp = runif(5)),
#'                cities = level(N = 5,
#'                               subways = rnorm(N, mean = 5)))
#'
#' @export
level <-
  function(ID_label,
           N = NULL,
           ...,
           by = NULL,
           data = NULL) {
    ## data is existing data to begin this level with

    dots <- quos(...)
    if ("data_internal_" %in% names(dots)) {
      data_internal_ <- eval_tidy(dots[["data_internal_"]])
      dots[["data_internal_"]] <- NULL
    } else {
      data_internal_ <- NULL
    }

    by <- substitute(by)
    if (!is.null(by)) {
      by <- as.character(by)
    }

    ID_label <- substitute(ID_label)
    if (!is.null(ID_label)) {
      ID_label <- as.character(ID_label)
    }

    if (is.null(data_internal_) & is.null(data)) {
      if (is.null(N)) {
        stop(paste0(
          "If you do not provide data to level",
          ID_label,
          ", please provide N."
        ))
      }
      if (length(N) > 1) {
        stop(paste0(
          "At the top level, ",
          ID_label,
          ", you must provide a single number to N."
        ))
      }
      # make IDs that are nicely padded
      data_internal_ <-
        data.frame(sprintf(paste0("%0", nchar(N), "d"), 1:N), stringsAsFactors = FALSE)
      colnames(data_internal_) <- ID_label

    } else {
      if (is.null(data)) {
        # either provide an existing ID_label or N if you don't provide custom data_internal_

        # if there is no ID variable, expand the dataset based on the commands in N
        if (!ID_label %in% colnames(data_internal_)) {

          # this check copied and pasted from purrr
          N <- eval(substitute(N), envir = data_internal_)

          data_internal_ <- expand_data_by_ID(data = data_internal_, ID_label = ID_label, N = N)

        } else {
          # otherwise assume you are adding variables to an existing level
          # defined by the level ID variable that exists in the data_internal_

          level_variables <- get_unique_variables_by_level(
            data = data_internal_, ID_label = ID_label)

          data <- unique(data_internal_[, unique(c(ID_label, level_variables)),
                                        drop = FALSE])

          options <- lang_modify(dots, data = data,
                                 N = NULL,
                                 ID_label = ID_label,
                                 existing_ID = TRUE)
          level_call <- quo(fabricate_data_single_level(!!!options))

          data <- eval_tidy(level_call)

          return(merge(
            data_internal_[, colnames(data_internal_)[!(colnames(data_internal_) %in%
                                      level_variables)], drop = FALSE],
            data,
            by = as.character(substitute(ID_label)),
            all = TRUE,
            sort = FALSE
          ))

        }

      } else {
        ## if they sent data start with that

        if (!is.null(N)) {
          stop(
            paste0(
              "Please provide level ",
              ID_label,
              " with either data or N, not both."
            )
          )
        }

        if (!is.null(data_internal_)) {
          data_internal_ <- merge(data_internal_,
                        data,
                        by = by,
                        all = TRUE,
                        sort = FALSE)
        } else {
          data_internal_ <- data
        }

      }

    }

    # now that data_internal_ is the right size, pass to "mutate", i.e., simulate data

    options <- lang_modify(dots, data = data_internal_,
                           N = NULL,
                           ID_label = ID_label)
    level_call <- quo(fabricate_data_single_level(!!!options))

    eval_tidy(level_call)

  }


get_unique_variables_by_level <- function(data, ID_label) {
  ## identify variables that do not vary within ID_label
  ## maybe there is a faster way to do this?
  level_variables <-
    sapply(colnames(data)[!colnames(data) %in% ID_label], function(i)
      max(tapply(data[, i], list(data[, ID_label]),
                 function(x)
                   length(unique(x)))) == 1)
  return(names(level_variables)[level_variables])
}

expand_data_by_ID <- function(data, ID_label, N) {
  if (typeof(N) %in% c("integer", "double") &&
      length(N) == 1) {
    data <- data[rep(1:nrow(data), each = N), , drop = FALSE]
  } else if (typeof(N) %in% c("integer", "double") &&
             length(N) > 1) {
    # check that the vector that is N is the right length, i.e the length of data_internal_
    if (length(N) != nrow(data)) {
      stop(
        paste0(
          "If you provide a vector to N for level",
          ID_label,
          ", it must be the length of the dataset at the level above it ",
          "in the hierarchy"
        )
      )
    }
    data <- data[rep(1:nrow(data), times = N), , drop = FALSE]
  } else if (class(N) == "function") {
    data <- data[rep(1:nrow(data), times = N()), , drop = FALSE]
  } else {
    stop(
      paste0(
        "Please provide level ",
        ID_label,
        " with N that is a vector, scalar, or function that generates a vector."
      )
    )
  }
}

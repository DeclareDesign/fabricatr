
#' Fabricate a Level of Data for Multi-Level Hierarchical Data
#'
#' @param ID_label variable name for ID variable, i.e. citizen_ID (optional)
#'
#' @param N number of units to draw in the level
#' @param ... Data generating arguments, such as \code{my_var = rnorm(N)}. You may also pass \code{level()} arguments, which define a level of a multi-level dataset. For example, you could send to \code{...} \code{level(my_level, var = rnorm)}. See examples.
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
#' @export
level <-
  function(ID_label,
           N = NULL,
           ...) {

    # handle data that is sent from higher levels of the hierarchy
    # this is done internally through data_internal_, which is passed through
    # the ...; users can send data to any level through data, but is handled
    # differently
    dots <- quos(...)
    if ("data_internal_" %in% names(dots)) {
      data_internal_ <- eval_tidy(dots[["data_internal_"]])
      dots[["data_internal_"]] <- NULL
    } else {
      data_internal_ <- NULL
    }

    ID_label <- substitute(ID_label)
    if (!is.null(ID_label)) {
      ID_label <- as.character(ID_label)
    }

    if (is.null(data_internal_)) {

      ## if data is not provided to fabricate_data, this part handles the case
      ##   of the top level, where data must be created for the first time
      if (is.null(N)) {
        stop(
          paste0(
            "At the top level, ",
            ID_label,
            ", you must provide N if you did not provide data to fabricate_data."
          )
        )
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

      # now that data_internal_ is the right size, pass to "mutate", i.e., simulate data

      options <- lang_modify(dots,
                             data = data_internal_,
                             N = NULL,
                             ID_label = ID_label)
      level_call <- quo(fabricate_data_single_level(!!!options))

      return(eval_tidy(level_call))

    } else {
      # at the second level, after data is created, or at the top level if data is provided
      #  to fabricate_data, there are two case:
      # 1. ID_label does not yet exist, in which case we create the level defined by ID_label by expanding dataset based on N
      # 2. ID label already exists, in which case we add variables to an existing level

      # if there is no ID variable, expand the dataset based on the commands in N
      if (!ID_label %in% colnames(data_internal_)) {
        N <- eval(substitute(N), envir = data_internal_)

        data_internal_ <-
          expand_data_by_ID(data = data_internal_,
                            ID_label = ID_label,
                            N = N)

        # now that data_internal_ is the right size, pass to "mutate", i.e., simulate data

        options <- lang_modify(dots,
                               data = data_internal_,
                               N = NULL,
                               ID_label = ID_label)
        level_call <- quo(fabricate_data_single_level(!!!options))

        return(eval_tidy(level_call))

      } else {
        # otherwise assume you are adding variables to an existing level
        # defined by the level ID variable that exists in the data_internal_

        # get the set of variable names that are unique within the level you are adding vars to
        #  so the new vars can be a function of existing ones
        level_variables <-
          get_unique_variables_by_level(data = data_internal_, ID_label = ID_label)

        # construct a dataset with only those variables at this level
        data <-
          unique(data_internal_[, unique(c(ID_label, level_variables)),
                                drop = FALSE])

        # set up
        options <- lang_modify(
          dots,
          data = data,
          N = NULL,
          ID_label = ID_label,
          existing_ID = TRUE
        )

        level_call <- quo(fabricate_data_single_level(!!!options))

        data <- eval_tidy(level_call)

        return(merge(
          data_internal_[, colnames(data_internal_)[!(colnames(data_internal_) %in%
                                                        level_variables)], drop = FALSE],
          data,
          by = ID_label,
          all = TRUE,
          sort = FALSE
        ))

      }

    }

  }

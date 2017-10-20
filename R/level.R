
#' Fabricate a Level of Data for Hierarchical Data
#'
#' @importFrom rlang quos eval_tidy quo lang_modify
#'
#' @rdname fabricate
#' @export
level <-
  function(N = NULL,
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

    if ("ID_label_" %in% names(dots)) {
      ID_label <- eval_tidy(dots[["ID_label_"]])
      dots[["ID_label_"]] <- NULL
    } else {
      stop("Please provide a name for the level, by specifying `your_level_name = level()` in fabricate.")
    }

    if (is.null(data_internal_)) {

      ## if data is not provided to fabricate, this part handles the case
      ##   of the top level, where data must be created for the first time
      return(fabricate_data_single_level(N=N, ID_label=ID_label, options=dots))

    } else {
      # at the second level, after data is created, or at the top level if data is provided
      #  to fabricate, there are two case:
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


#' Fabricate a Level of Data for Hierarchical Data
#'
#' @importFrom rlang quos eval_tidy quo lang_modify
#'
#' @rdname fabricate
#' @export
level <-
  function(N = NULL,
           ...) {

    # The dots take all the arguments at the current level, as well as
    # the provisional working data frame, which is injected at a higher level
    dots <- quos(...)

    # If we were passed a working data frame, let's move it into our current level
    if ("data_internal_" %in% names(dots)) {
      data_internal_ <- eval_tidy(dots[["data_internal_"]])
      dots[["data_internal_"]] <- NULL
    } else {
      data_internal_ <- NULL
    }

    # If we were passed an ID for this level (and if not, something went wrong)
    # pass it through transparently
    if ("ID_label_" %in% names(dots)) {
      ID_label <- eval_tidy(dots[["ID_label_"]])
      dots[["ID_label_"]] <- NULL
    } else {
      stop("Please provide a name for the level, by specifying `your_level_name = level()` in fabricate.")
    }

    if (is.null(data_internal_)) {
      # We're at a top level case with no provided data
      return(fabricate_data_single_level(N=N, ID_label=ID_label, options=dots))
    } else {
      # We're at a second level case, or a top-level case with provided data. Two things to proceed:
      # 1. ID_label does not yet exist, in which case we create the level defined by ID_label by expanding dataset based on N
      # 2. ID label already exists, in which case we add variables to an existing level

      # If we are getting a new ID label, it's a new level.
      if (!ID_label %in% colnames(data_internal_)) {
        # Evaluate how many N we need
        N <- eval(substitute(N), envir = data_internal_)

        # Expand the working data frame using ID_label
        data_internal_ <-
          expand_data_by_ID(data = data_internal_,
                            ID_label = ID_label,
                            N = N)

        # Having expanded and added the ID, we can fab the rest of the variables
        return(fabricate_data_single_level(data_internal_, NULL, ID_label, options=dots))

      } else {
        # The level already exists, we are adding variables to it.

        # Which variables could we possibly care about in this level call?
        unique_variables_to_write_to = unname(unlist(get_symbols_from_quosure(dots)))
        # Remove the level variable from consideration -- we know this unique
        # conditional on itself by definition
        unique_variables_to_write_to = setdiff(unique_variables_to_write_to, ID_label)

        # Subset the working data frame to data that matters by the level we care about
        # based on the level call we have.
        level_variables <-
          get_unique_variables_by_level(data = data_internal_,
                                        ID_label = ID_label,
                                        superset=unique_variables_to_write_to)

        merged_set = unique(c(ID_label, level_variables))

        data <-
          unique(data_internal_[, merged_set[merged_set != ""],
                                drop = FALSE])

        # Now, fabricate the new variables at this level

        data <- fabricate_data_single_level(data, NULL, ID_label, existing_ID = TRUE, options=dots)

        # Now, merge the new data frame into the old one to recover the variables we previously dropped
        return(merge(
          data_internal_[,
                         !(colnames(data_internal_) %in% level_variables),
                         drop = FALSE],
          data,
          by = ID_label,
          all = TRUE,
          sort = FALSE
        ))

      }

    }

  }

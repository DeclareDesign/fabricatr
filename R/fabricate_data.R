




#' @export
fabricate_data <- function(..., N = NULL, ID_label = NULL, data = NULL) {

  ## needs to be changed to lazy eval

  options <- eval(substitute(alist(...)))

  options_text <- paste(substitute(options))

  # check if all the options are level calls.
  if (all(sapply(options_text, function(x)
    startsWith(x, "level(")))) {

    # If we don't have data yet, make the first level.
    if (is.null(data)) {
      # Do a sweet switcheroo with the level names if applicable.
      if (is.null(options[[1]]$ID_label)) {
        options[[1]]$ID_label <- names(options)[1]
      }
      data <- eval(options[[1]])
    }

    # iff there are multiple levels, please to continue
    if (length(options) > 1) {

      for (i in 2:length(options)) {
        # Pop the data from the previous level in the current call
        options[[i]]$data <- data

        # Also do a sweet switcheroo with the level names if applicable.
        if (is.null(options[[i]]$ID_label)) {
          options[[i]]$ID_label <- names(options)[i]
        }
        # update the current data
        data <- eval(options[[i]])

      }
    }

    return(data)

  } else {
    # Sometimes life is simple
    fabricate_data_single_level_(data = data, N = N, ID_label = ID_label, dots_capture(...))
  }
}


#' @importFrom lazyeval f_eval as_f_list
#' @export
fabricate_data_single_level_ <- function(data = NULL, N = NULL, ID_label = NULL, args) {
  if (sum(!is.null(data),!is.null(N)) != 1) {
    stop("Please supply either a data.frame or N and not both.")
  }

  if (is.null(data)) {
    # make IDs that are nicely padded
    data <-
      data.frame(sprintf(paste0("%0", nchar(N), "d"), 1:N), stringsAsFactors = FALSE)

    # this creates column names from ID_label
    # note if ID_label is NULL that the ID column name is just "ID" -- so safe
    colnames(data) <- paste(c(ID_label, "ID"), collapse = "_")
  } else {
    N <- nrow(data)
    if (!is.null(ID_label)) {
      data[, paste(c(ID_label, "ID"), collapse = "_")] <-
        sprintf(paste0("%0", nchar(N), "d"), 1:N)
    }
  }

  args <- as_f_list(args)

  # inspired directly by lazyeval vignette
  for (nm in names(args)) {

    # This line probs bad, as it copies the data....
    # Need to do it every time, so that the next variable can depend on old vars.
    data_list <- as.list(data)
    data_list$N <- N

    data[[nm]] <- f_eval(args[[nm]], data_list)
  }
  rownames(data) <- NULL
  return(data)
}


#' @importFrom lazyeval dots_capture
#' @export
fabricate_data_single_level <- function(data = NULL, N = NULL, ID_label = NULL, ...) {
  fabricate_data_single_level_(data = data, N = N, ID_label = ID_label, dots_capture(...))
}

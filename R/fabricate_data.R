
#' @export
fabricate_data <- function(...) {
  options <- eval(substitute(alist(...)))
  options_text <- paste(options)

  # change the ones that are calls to character strings for fabricate_data_
  is_call <- sapply(options, class) == "call"
  options[is_call] <- as.list(paste0(names(options[is_call]), " = ", paste(options[is_call])))
  names(options)[is_call] <- ""

  do.call(fabricate_data_, args = options)
}


#' @export
fabricate_data_ <-
  function(...){
    options <- list(...)

    # Say, are there any level calls in there?
    is_level_calls <- sapply(options[sapply(options, is.character)],
                             function(x) startsWith(x, prefix = "level("))

    if (all(is_level_calls)) {

      # THiS IS SOME JANK-CITY BECAUSE I WANNA PIPE TO THA DOTZ
      options[2:length(options)] <-
        sapply(options[2:length(options)], function(x) paste0(substr(x, 1, nchar(x) - 1 ), ", data = .)"))

      options <- paste(options, collapse = " %>% ")

      return(eval(parse(text = options)))

    } else{
      return(fabricate_data_single_level_(... = ...))
    }

  }


#' @export
fabricate_data_single_level_ <-
  function(...,
           N = NULL,
           data = NULL,
           ID_label = NULL
  ) {

    # Checks
    if (sum(!is.null(data), !is.null(N)) != 1) {
      stop("Please supply either a data.frame or N and not both.")
    }


    # IDs
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

    # Deal with quoted expressions!

    options <- list(...)

    ## we need to split the list of options by equal signs
    ## save the lhs as expressions_names, the rhs as expressions
    expressions_list <- lapply(lapply(options, function(x) strsplit(x, split = "=")[[1]]), trimws)
    expressions <- lapply(expressions_list, `[[`, 2)
    expression_names <- lapply(expressions_list, `[[`, 1)

    ## TO DO: Make a good error for when ppl dont give their ...s as
    # things that look like
    # "Y = 3*X"

    for (i in 1:length(expressions)) {
      data_environment <- list2env(data)
      data[, expression_names[[i]]] <-
        eval(parse(text = expressions[[i]]), envir = data_environment)
    }
    rownames(data) <- NULL
    return(data)
  }

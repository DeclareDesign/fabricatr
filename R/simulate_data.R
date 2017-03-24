
#' @export
#'
#' @param ...
simulate_data <- function(...) {
  options <- eval(substitute(alist(...)))
  options_text <- paste(options)


  # just trying to find our whether everything in the dots is level() or not!
  # OPEN TO OTHER SOLUTIONS
  is_level_calls <-
    sapply(options_text, function(x)
      startsWith(x, prefix = "level("))

  # THiS IS SOME JANK-CITY BECAUSE I WANNA PIPE TO THA DOTZ
  options_text[2:length(options_text)] <-
    sapply(options_text[2:length(options_text)], function(x)paste0(substr(x, 1, nchar(x) - 1 ), ", data = .)"))

  if (all(is_level_calls)) {
    options_text <- paste(options_text, collapse = " %>% ")

    return(eval(parse(text = options_text)))

  } else{
    return(simulate_data_internal(... = ...))
  }
}


simulate_data_internal <-
  function(data = NULL,
           N = NULL,
           ID_label = NULL,
           ...) {
    if ((is.null(data) & is.null(N)) | (!is.null(data) & !is.null(N))) {
      stop("Please supply either a data.frame or N and not both.")
    }

    if (is.null(data)) {
      # make IDs that are nicely padded
      data <-
        data.frame(sprintf(paste0("%0", nchar(N), "d"), 1:N), stringsAsFactors = FALSE)
      colnames(data) <- paste(c(ID_label, "ID"), collapse = "_")
    } else {
      N <- nrow(data)
      if (!is.null(ID_label)) {
        data[, paste(c(ID_label, "ID"), collapse = "_")] <-
          sprintf(paste0("%0", nchar(N), "d"), 1:N)
      }
    }

    expressions <- eval(substitute(alist(...)))
    expression_names <- names(expressions)

    if (any(is.null(expression_names)) |
        any(expression_names == "")) {
      stop("Please name all of your variables, i.e. var1 = rnorm(N).")
    }

    for (i in 1:length(expressions)) {
      data_environment <- list2env(data)
      data[, expression_names[i]] <-
        eval(expressions[[i]], envir = data_environment)
    }
    rownames(data) <- NULL
    return(data)
  }

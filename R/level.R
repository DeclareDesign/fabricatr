#' @param level_name
#'
#' @param N
#' @param data
#' @param ...
#'
#' @export
#'
level <- function(level_name, N = NULL, data = NULL, ...){

  level_name <- as.character(substitute(level_name))

  if (is.null(data)) {

    if (is.null(N)) {
      stop(paste0("If you do not provide data to level", level_name, ", please provide N."))
    }
    # make IDs that are nicely padded
    data <- data.frame(sprintf(paste0("%0", nchar(N), "d"), 1:N), stringsAsFactors = FALSE)
    colnames(data) <- paste(c(level_name, "ID"), collapse = "_")
  } else {

    # this check copied and pasted from purrr
    if (typeof(N) %in% c("integer", "double") && length(N) == 1) {
      data <- data[rep(1:nrow(data), each = N), ]
    } else if (typeof(N) %in% c("integer", "double") && length(N) > 1) {
      # check that the vector that is N is the right length, i.e the length of data
      if (length(N) != nrow(data)) {
        stop(paste0("If you provide a vector to N for level",
                    level_name,
                    ", it must be the length of the dataset at the level above it in the heirarchy."))
      }
      data <- data[rep(1:nrow(data), times = N),]
    } else if (class(N) == "function") {
      data <- data[rep(1:nrow(data), times = N()),]
    } else {
      stop(paste0("Please provide level ", level_name, " with N that is a vector, scalar, or function that generates a vector."))
    }
  }

  # now that data is the right size, pass to "mutate", i.e., simulate data

  simulate_data_internal(data = data, ID_label = level_name, ... = ...)
}

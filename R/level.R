
#' @export
level <- function(ID_label, N = NULL, ..., data = NULL){

  #ID_label <- deparse(substitute(ID_label))
  if (is.null(data)) {

    if (is.null(N)) {
      stop(paste0("If you do not provide data to level", ID_label, ", please provide N."))
    }
    # make IDs that are nicely padded
    data <- data.frame(sprintf(paste0("%0", nchar(N), "d"), 1:N), stringsAsFactors = FALSE)
    colnames(data) <- paste(c(ID_label, "ID"), collapse = "_")
  } else {

    # this check copied and pasted from purrr
    if (typeof(N) %in% c("integer", "double") && length(N) == 1) {
      data <- data[rep(1:nrow(data), each = N), ]
    } else if (typeof(N) %in% c("integer", "double") && length(N) > 1) {
      # check that the vector that is N is the right length, i.e the length of data
      if (length(N) != nrow(data)) {
        stop(paste0("If you provide a vector to N for level",
                    ID_label,
                    ", it must be the length of the dataset at the level above it in the heirarchy."))
      }
      data <- data[rep(1:nrow(data), times = N),]
    } else if (class(N) == "function") {
      data <- data[rep(1:nrow(data), times = N()),]
    } else {
      stop(paste0("Please provide level ", ID_label, " with N that is a vector, scalar, or function that generates a vector."))
    }
  }

  # now that data is the right size, pass to "mutate", i.e., simulate data

  fabricate_data_single_level_(data = data, N = NULL,
                               ID_label = ID_label, dots_capture(...))
}





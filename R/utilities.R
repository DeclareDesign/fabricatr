format_num <- function(x, digits = 3) {
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
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

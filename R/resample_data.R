#' Magic constant: pass all units at a resampling level
#'
#' Use \code{ALL} in the \code{N} argument of \code{resample_data} to keep
#' every unit at a given hierarchical level while still resampling inner
#' levels.
#'
#' @export
ALL <- -20171101L

#' Resample a data frame, with optional hierarchical cluster resampling
#'
#' For simple bootstrap resampling, call \code{resample_data(data)} with no
#' other arguments. For cluster resampling, supply \code{N} and
#' \code{ID_labels}. For multi-level hierarchical resampling, supply vectors
#' for both.
#'
#' @param data A data frame.
#' @param N Resample size. May be:
#'   \itemize{
#'     \item Missing: resample \code{nrow(data)} units (standard bootstrap).
#'     \item A single unnamed number, with no \code{ID_labels}: resample that
#'       many rows with replacement.
#'     \item A named integer vector: names are cluster ID column names (from
#'       outermost to innermost), values are the number of units to resample
#'       at each level. Use the constant \code{ALL} to pass through all units
#'       at a level.
#'     \item An unnamed integer vector: same as above but supply \code{ID_labels}
#'       separately.
#'   }
#' @param ID_labels Character vector of cluster ID column names (outermost to
#'   innermost). Required when \code{N} is unnamed.
#' @param unique_labels Logical. When \code{TRUE}, adds a
#'   \code{<ID>_unique} column with globally unique labels after resampling
#'   (useful for multi-level bootstraps where the same cluster is drawn
#'   multiple times). Default \code{FALSE}.
#'
#' @return A tibble with row names reset to \code{NULL}.
#'
#' @examples
#' # Simple bootstrap
#' df <- fabricate(N = 50, Y = rnorm(N))
#' boot <- resample_data(df)
#' nrow(boot)
#'
#' # Cluster bootstrap: resample 10 of 20 clusters
#' clustered <- fabricate(
#'   clusters = add_level(N = 20, gdp = rnorm(N)),
#'   units    = nest_level(N = 5, Y = gdp + rnorm(N))
#' )
#' resample_data(clustered, N = c(clusters = 10))
#'
#' @importFrom stats ave
#' @export
resample_data <- function(data, N, ID_labels = NULL, unique_labels = FALSE) {
  data <- tibble::as_tibble(data)

  if (missing(N) && is.null(ID_labels)) {
    out <- data[sample.int(nrow(data), nrow(data), replace = TRUE), , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }

  # A bare, unnamed N with no ID_labels is a row-level bootstrap of size N,
  # which is what fabricatr 1.x returned for resample_data(data, N = 40).
  if (is.null(ID_labels) && is.null(names(N))) {
    if (length(N) != 1) {
      stop("An unnamed `N` of length > 1 needs `ID_labels` naming one level per element.")
    }
    out <- data[sample.int(nrow(data), N, replace = TRUE), , drop = FALSE]
    rownames(out) <- NULL
    return(tibble::as_tibble(out))
  }

  # Reconcile names vs ID_labels
  if (!is.null(names(N)) && !is.null(ID_labels)) {
    stop("Provide level names either in `N` (as names) or in `ID_labels`, not both.")
  }
  if (!is.null(names(N))) ID_labels <- names(N)
  if (length(N) != length(ID_labels)) {
    stop("`N` and `ID_labels` must have the same length.")
  }
  if (any(!ID_labels %in% names(data))) {
    stop("ID_labels not found in data: ",
         paste(setdiff(ID_labels, names(data)), collapse = ", "))
  }

  out <- resample_recursive(data, N, ID_labels,
                            unique_labels = unique_labels, prefix = "")

  # The recursion attaches each level's unique-label column on the way back up,
  # so they come out innermost first. fabricatr orders them outermost first,
  # after the original columns.
  if (unique_labels) {
    uniq <- paste0(ID_labels, "_unique")
    out  <- out[, c(setdiff(names(out), uniq), uniq), drop = FALSE]
  }

  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# Recursive helper ------------------------------------------------------------

resample_recursive <- function(data, N, ID_labels, unique_labels, prefix) {
  id_col <- ID_labels[1]
  n_this <- N[1]

  groups   <- split(seq_len(nrow(data)), data[[id_col]])
  group_ids <- names(groups)
  k         <- length(group_ids)

  if (n_this == ALL) {
    chosen <- seq_len(k)
  } else {
    chosen <- sample.int(k, n_this, replace = TRUE)
  }

  if (unique_labels) {
    raw_labels <- group_ids[chosen]
    new_labels <- make_unique_labels(raw_labels, prefix)
  }

  if (length(N) == 1) {
    row_idx <- unlist(groups[chosen], use.names = FALSE)
    out <- data[row_idx, , drop = FALSE]
    if (unique_labels) {
      repeats <- lengths(groups[chosen])
      out[[paste0(id_col, "_unique")]] <- rep(new_labels, times = repeats)
    }
    return(out)
  }

  chunks <- lapply(seq_along(chosen), function(i) {
    gi    <- chosen[i]
    slice <- data[groups[[gi]], , drop = FALSE]
    pfx   <- if (unique_labels) new_labels[i] else prefix
    sub   <- resample_recursive(slice, N[-1], ID_labels[-1],
                                unique_labels = unique_labels, prefix = pfx)
    if (unique_labels) sub[[paste0(id_col, "_unique")]] <- pfx
    sub
  })

  dplyr::bind_rows(chunks)
}

# A label is <outer prefix>_<level id>_<nth time this id was drawn>, with the
# prefix omitted at the outermost level. Pasting an empty prefix in as a
# component instead would leave a leading underscore, and a prefix that already
# ends in one would double it.
make_unique_labels <- function(labels, prefix) {
  counts <- ave(labels, labels, FUN = seq_along)
  if (nchar(prefix) > 0) {
    paste(prefix, labels, counts, sep = "_")
  } else {
    paste(labels, counts, sep = "_")
  }
}

#' Fabricate a data frame
#'
#' The main entry point for fabricatrZero. Builds a data frame column by column,
#' supporting hierarchical levels, multi-column outputs, and starting from
#' existing data.
#'
#' @param N Number of rows. Made available as a scalar integer \code{N} inside
#'   all column expressions; it does not appear as a column in the output
#'   unless you write it explicitly (e.g. \code{n_obs = N}).
#' @param ... Column definitions or level calls (\code{add_level},
#'   \code{nest_level}, \code{declare_level}, \code{cross_levels},
#'   \code{link_levels}, \code{modify_level}, \code{potential_outcomes},
#'   \code{draw_multivariate}).
#' @param ID_label Name prefix for auto-generated level ID columns.
#' @param data Optional existing data frame to start from.
#'
#' @return A tibble.
#'
#' @examples
#' # Flat fabrication
#' fabricate(N = 100, Y = rnorm(N), X = rbinom(N, 1, 0.5))
#'
#' # Hierarchical: 20 villages, 5 citizens each
#' fabricate(
#'   villages = add_level(N = 20, v_income = rnorm(N)),
#'   citizens = nest_level(N = 5, income = v_income + rnorm(N))
#' )
#'
#' # Starting from existing data
#' df <- data.frame(x = 1:10)
#' fabricate(data = df, y = x ^ 2)
#'
#' @export
fabricate <- function(N = NULL, ..., ID_label = "ID", data = NULL) {
  dots <- rlang::enquos(...)
  fabricate_impl(N = N, dots = dots, data = data)
}

# Internal: called by DeclareDesignZero's make_fabricate_step with a
# pre-captured quosures list, avoiding double-quoting from !!!-injection.
#' @keywords internal
fabricate_with_dots <- function(data = NULL, dots) {
  # Extract N if it was captured as a named quosure in dots (flat case)
  N <- NULL
  N_idx <- which(names(dots) == "N")
  if (length(N_idx) > 0L) {
    N <- rlang::eval_tidy(dots[[N_idx[1L]]])
    dots <- dots[-N_idx[1L]]
  }
  fabricate_impl(N = N, dots = dots, data = data)
}

fabricate_impl <- function(N = NULL, dots, data = NULL) {
  # Maintain a plain named list throughout — far cheaper than tibble for
  # intermediate operations. Convert to tibble exactly once at the end.
  if (!is.null(data)) {
    lst      <- as.list(tibble::as_tibble(data))
    N_inject <- length(lst[[1L]])
  } else if (!is.null(N)) {
    N_val    <- as.integer(N)
    lst      <- list()
    N_inject <- N_val
  } else {
    lst      <- list()
    N_inject <- NULL
  }

  level_registry <- list()

  for (i in seq_along(dots)) {
    nm <- names(dots)[[i]]

    mask <- lst
    if (!is.null(N_inject) && !"N" %in% names(mask)) mask[["N"]] <- N_inject

    val <- rlang::eval_tidy(dots[[i]], data = mask)

    if (inherits(val, "fabricatr_level")) {
      lst <- execute_level(val, lst, N_inject, nm, level_registry)
      if (nchar(nm) > 0) level_registry[[nm]] <- lst
      N_inject <- if (length(lst) > 0L) length(lst[[1L]]) else N_inject
    } else if (is.data.frame(val)) {
      # Recycle length-1 columns (e.g. potential_outcomes with constant RHS)
      n_rows <- if (length(lst) > 0L) length(lst[[1L]]) else N_inject %||% 0L
      for (j in seq_along(val)) {
        v <- val[[j]]
        if (length(v) == 1L && n_rows > 1L) v <- rep(v, n_rows)
        lst[[names(val)[[j]]]] <- v
      }
    } else if (nchar(nm) > 0) {
      lst[[nm]] <- val
    }
  }

  list_to_df(lst)
}

# Converts a named list of equal-length vectors to a tibble.
# Using data.frame() is faster than tibble() for construction;
# as_tibble() then adds the tbl_df class cheaply.
list_to_df <- function(lst) {
  if (length(lst) == 0L) return(tibble::tibble())
  tibble::as_tibble(lst)
}

# Internal dispatcher ----

execute_level <- function(level, lst, N_inject, nm, level_registry) {
  switch(level$type,
    add     = if (length(lst) > 0L)
                execute_nest_level(level, lst, N_inject, nm)
              else
                execute_add_level(level, nm),
    declare = execute_add_level(level, nm),
    nest    = execute_nest_level(level, lst, N_inject, nm),
    cross   = execute_cross_level(level, level_registry, nm),
    link    = execute_link_level(level, level_registry, nm),
    modify  = execute_modify_level(level, lst, N_inject),
    stop("Unknown fabricatrZero level type: ", level$type)
  )
}

# Sequential eval helper ----
eval_dots_into_list <- function(dots, base_list, inner_N = NULL) {
  lst <- base_list
  if (!is.null(inner_N) && !"N" %in% names(lst)) lst[["N"]] <- inner_N

  for (i in seq_along(dots)) {
    nm  <- names(dots)[[i]]
    val <- rlang::eval_tidy(dots[[i]], data = lst)

    if (nchar(nm) > 0) {
      lst[[nm]] <- val
    } else if (is.data.frame(val)) {
      for (j in seq_along(val)) lst[[names(val)[[j]]]] <- val[[j]]
    }
  }
  lst
}

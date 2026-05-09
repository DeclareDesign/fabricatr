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
  # df: working data frame (starts with correct row count, 0 columns)
  # N_inject: scalar made available as "N" in every data mask
  if (!is.null(data)) {
    df       <- tibble::as_tibble(data)
    N_inject <- nrow(df)
  } else if (!is.null(N)) {
    N_val    <- as.integer(N)
    df       <- tibble::tibble(.rows = N_val)
    N_inject <- N_val
  } else {
    df       <- tibble::tibble()
    N_inject <- NULL
  }

  # Registry of named top-level data frames for cross_levels / link_levels
  level_registry <- list()

  for (i in seq_along(dots)) {
    nm <- names(dots)[[i]]

    # Build data mask: current columns + N (injected unless user already has it)
    mask <- as.list(df)
    if (!is.null(N_inject) && !"N" %in% names(mask)) mask[["N"]] <- N_inject

    val <- rlang::eval_tidy(dots[[i]], data = mask)

    if (inherits(val, "fabricatr_level")) {
      df <- execute_level(val, df, nm, level_registry)
      if (nchar(nm) > 0) level_registry[[nm]] <- df
      N_inject <- nrow(df)
    } else if (is.data.frame(val)) {
      # Multi-column output: potential_outcomes, draw_multivariate, etc.
      df <- dplyr::bind_cols(df, val)
    } else if (nchar(nm) > 0) {
      df[[nm]] <- val
    }
  }

  df
}  # end fabricate_impl

# Internal dispatcher ----

execute_level <- function(level, current_df, nm, level_registry) {
  switch(level$type,
    # add_level auto-nests when a hierarchy already exists (nrow > 0),
    # exactly matching fabricatr's default nest = TRUE behaviour.
    add     = if (nrow(current_df) > 0L)
                execute_nest_level(level, current_df, nm)
              else
                execute_add_level(level, nm),
    declare = execute_add_level(level, nm),
    nest    = execute_nest_level(level, current_df, nm),
    cross   = execute_cross_level(level, level_registry, nm),
    link    = execute_link_level(level, level_registry, nm),
    modify  = execute_modify_level(level, current_df),
    stop("Unknown fabricatrZero level type: ", level$type)
  )
}

# Sequential eval helper: builds up a list as a growing data mask ----
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

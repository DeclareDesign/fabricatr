#' Fabricate a data frame
#'
#' The main entry point for fabricatr. Builds a data frame column by column,
#' supporting hierarchical levels, multi-column outputs, and starting from
#' existing data.
#'
#' @param ... Column definitions or level calls (\code{add_level},
#'   \code{nest_level}, \code{declare_level}, \code{cross_levels},
#'   \code{link_levels}, \code{modify_level}, \code{potential_outcomes},
#'   \code{draw_multivariate}). \code{N} must be supplied by name.
#' @param N Number of rows. Made available as a scalar integer \code{N} inside
#'   all column expressions; it does not appear as a column in the output
#'   unless you write it explicitly (e.g. \code{n_obs = N}).
#' @param ID_label Name of the unit ID column created alongside \code{N}.
#'   Defaults to \code{"ID"}; set to \code{NA} to suppress it. Ignored when
#'   \code{data} is supplied or when the data frame is built from levels
#'   (each level's ID column is named after the level).
#' @param data Optional existing data frame to start from.
#'
#' @section ID column type:
#' ID columns are zero-padded character strings, as in fabricatr, so that an
#' ID is a label rather than a quantity. The padding keeps character sort
#' order matching numeric order, and the character type keeps a cluster ID
#' from being read as a linear term by a model formula. The padding width is
#' set by the number of units at that level, so a frame can hold
#' \code{clusters} running \code{"1"} to \code{"4"} alongside \code{units}
#' running \code{"01"} to \code{"12"}.
#'
#' @section Counting the rows a level is building:
#' \code{N} names the number of rows the current level is building, so
#' \code{rnorm(N)} draws one value per row. \code{n()} answers the same
#' question as a function call and is available in every declaration. Two
#' things make it worth reaching for. It cannot be confused with a parameter
#' of the design: \code{N} is a name, so a design that also has an \code{N}
#' in the workspace can leave a reader unsure which one an expression reads,
#' and \code{redesign()} rebinds names, where \code{n()} always means the
#' rows in hand. And a data set carrying a column called \code{n} keeps it:
#' \code{sum(n)} reads the column and \code{n()} reads the count.
#'
#' @return A tibble.
#'
#' @examples
#' # Flat fabrication
#' fabricate(N = 100, Y = rnorm(N), X = rbinom(N, 1, 0.5))
#'
#' # n() is the number of rows the level is building
#' fabricate(N = 10, Y = rnorm(n()))
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
#' @importFrom rlang %||%
#' @export
fabricate <- function(..., N = NULL, ID_label = "ID", data = NULL) {
  dots <- rlang::enquos(...)
  fabricate_impl(N = N, dots = dots, data = data, ID_label = ID_label)
}

# Internal: called by DeclareDesign's make_fabricate_step with a
# pre-captured quosures list, avoiding double-quoting from !!!-injection.
#' @keywords internal
fabricate_with_dots <- function(data = NULL, dots, ID_label = "ID") {
  # Extract N if it was captured as a named quosure in dots (flat case)
  N <- NULL
  N_idx <- which(names(dots) == "N")
  if (length(N_idx) > 0L) {
    N <- rlang::eval_tidy(dots[[N_idx[1L]]])
    dots <- dots[-N_idx[1L]]
  }
  fabricate_impl(N = N, dots = dots, data = data, ID_label = ID_label)
}

#' A data mask that also answers `n()`
#'
#' `N` is a column of the mask, so a declaration that reads it gets whatever
#' the level is building. `n()` answers the same question as a function call,
#' which is worth having for two reasons: it cannot be mistaken for a
#' parameter of the design and rebound by `redesign()`, and a data set that
#' happens to carry a column called `n` still reads as that column, because R
#' skips non-function bindings when it looks up a call.
#'
#' The size is bound between the data and the code's own environment, so the
#' columns win over it and it wins over a `n()` from an attached package.
#' A declaration written where `n` is already an ordinary variable is left
#' alone: that variable keeps its meaning, and `n()` there is whatever it was
#' before. Shadowing it would silently change designs that use `n` as a name.
#'
#' @keywords internal
#' @noRd
level_mask <- function(data, size = NULL, env = NULL) {
  helpers <- rlang::new_environment()
  if (!name_holds_a_value(env, "n")) {
    rlang::env_bind(helpers, n = function() {
      if (is.null(size)) {
        stop("n() has no level to count here: nothing is being built.",
             call. = FALSE)
      }
      size
    })
  }
  nms <- names(data) %||% character(0)
  if (length(data) && (anyDuplicated(nms) || !all(nzchar(nms)))) {
    rlang::abort("`data` must be uniquely named but has duplicate columns")
  }
  data_env <- rlang::as_environment(data, parent = helpers)
  mask <- rlang::new_data_mask(data_env, top = helpers)
  mask$.data <- rlang::as_data_pronoun(data_env)
  mask
}

#' Whether a name already means something other than a function where the
#' declaration was written
#'
#' @keywords internal
#' @noRd
name_holds_a_value <- function(env, name) {
  if (!rlang::is_environment(env)) return(FALSE)
  found <- tryCatch(rlang::env_get(env, name, default = NULL, inherit = TRUE),
                    error = function(e) NULL)
  !is.null(found) && !is.function(found)
}

#' Evaluate one declaration against a level's data
#'
#' @keywords internal
#' @noRd
eval_in_level <- function(quo, data, size = NULL) {
  rlang::eval_tidy(quo, data = level_mask(data, size, rlang::quo_get_env(quo)))
}

fabricate_impl <- function(N = NULL, dots, data = NULL, ID_label = "ID") {
  # Maintain a plain named list throughout, which is far cheaper than a tibble
  # for intermediate operations. Convert to tibble exactly once at the end.
  if (!is.null(data) && !is.null(N)) {
    # fabricatr 1.0.2 refuses this outright, and it is worth refusing: the row
    # count is already fixed by the data, so `N` can only be read as an attempt
    # to make a column called `N`, which is a name the data mask owns. Silently
    # dropping it is how `declare_model(N = m, ...) + declare_model(N = 2.5)`
    # ran clean and did nothing.
    stop("`N` cannot be given alongside existing data.\n",
         "  The data already fix the number of rows. Do exactly one of:\n",
         "  a level call, with or without data; existing data plus new ",
         "variables; or `N` alone.", call. = FALSE)
  }
  if (!is.null(data)) {
    lst      <- as.list(tibble::as_tibble(data))
    N_inject <- length(lst[[1L]])
  } else if (!is.null(N)) {
    N_val    <- validate_n(N)
    lst      <- list()
    N_inject <- N_val
  } else {
    lst      <- list()
    N_inject <- NULL
  }

  # A flat fabricate() gets a unit ID column. Levels name their own ID columns,
  # so the flat ID is suppressed as soon as any level call runs. It is injected
  # into the data mask (so expressions can reference it) and prepended to the
  # output at the end.
  use_id <- is.null(data) && !is.null(N) &&
    !is.null(ID_label) && !is.na(ID_label) && nzchar(ID_label)
  id_vec <- if (use_id) make_ids(N_inject) else NULL

  level_registry <- list()
  saw_level <- FALSE

  for (i in seq_along(dots)) {
    nm <- names(dots)[[i]]

    mask <- lst
    if (!is.null(N_inject) && !"N" %in% names(mask)) mask[["N"]] <- N_inject
    if (!is.null(id_vec) && !saw_level && !ID_label %in% names(mask)) {
      mask[[ID_label]] <- id_vec
    }

    val <- eval_in_level(dots[[i]], mask, N_inject)

    if (inherits(val, "fabricatr_level")) {
      saw_level <- TRUE
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
      lst <- store_column(lst, nm, val,
                          function(v, cn) recycle_to_n(v, N_inject))
    } else {
      stop_unnamed_expression(i, dots[[i]], val, "fabricate()",
                              positional_n = is.null(N) && is.null(data))
    }
  }

  if (use_id && !saw_level && !ID_label %in% names(lst)) {
    lst <- c(stats::setNames(list(id_vec), ID_label), lst)
  }

  list_to_df(lst)
}

# A length-1 column is recycled to N as soon as it is stored, not at output
# time, because the list is also the data mask for every later expression in
# the level. Deferring it lets a constant column reach a later expression as a
# scalar, so `coordination = "high"` followed by
# `if_else(coordination == "high", tau_1, tau_2)` fails on a size mismatch
# where fabricatr, which recycles eagerly, succeeds.
recycle_to_n <- function(val, n) {
  if (!is.null(n) && length(val) == 1L && n > 1L) rep(val, n) else val
}

# A multi-column matrix becomes one column per matrix column, named X.1, X.2,
# ... as data.frame() would name them, and the split happens as the column is
# stored rather than on the way out. Keeping the matrix whole until output
# leaves `X = matrix(...)` followed by `Y = X.1` unable to see X.1, which is
# fabricatr#188.
split_matrix_column <- function(nm, val) {
  if (!is.matrix(val)) return(stats::setNames(list(val), nm))
  if (ncol(val) == 1L) return(stats::setNames(list(val[, 1L]), nm))
  suffix <- colnames(val)
  if (is.null(suffix)) suffix <- seq_len(ncol(val))
  stats::setNames(lapply(seq_len(ncol(val)), function(j) val[, j]),
                  paste(nm, suffix, sep = "."))
}

# `fix` is applied to each resulting column, so a length rule is enforced per
# column rather than against the whole matrix.
store_column <- function(lst, nm, val, fix = function(v, nm) v) {
  cols <- split_matrix_column(nm, val)
  for (cn in names(cols)) lst[[cn]] <- fix(cols[[cn]], cn)
  lst
}

# Converts a named list of equal-length vectors to a tibble.
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
    stop("Unknown fabricatr level type: ", level$type)
  )
}

# Sequential eval helper ----
eval_dots_into_list <- function(dots, base_list, inner_N = NULL) {
  lst <- base_list
  if (!is.null(inner_N) && !"N" %in% names(lst)) lst[["N"]] <- inner_N

  for (i in seq_along(dots)) {
    nm  <- names(dots)[[i]]
    val <- eval_in_level(dots[[i]], lst, inner_N)

    if (nchar(nm) > 0) {
      lst <- store_column(lst, nm, val,
                          function(v, cn) recycle_to_n(v, inner_N))
    } else if (is.data.frame(val)) {
      for (j in seq_along(val))
        lst[[names(val)[[j]]]] <- recycle_to_n(val[[j]], inner_N)
    } else {
      stop_unnamed_expression(i, dots[[i]], val, "this level")
    }
  }
  lst
}

# An expression with no name has nowhere to go. fabricatr 1.x failed on it
# with an indexing error from deep inside; dropping it silently, which is
# what happened here for a while, is worse, because `fabricate(100, ...)`
# then reads as a design with no rows. The unnamed things that do have a
# meaning, a level call and a multi-column result such as
# `potential_outcomes()`, are handled before this is reached.
stop_unnamed_expression <- function(i, quo, val, where, positional_n = FALSE) {
  looks_like_n <- positional_n && is.numeric(val) && length(val) == 1L
  stop("Every column needs a name. Expression ", i, " in ", where, ", `",
       rlang::as_label(quo), "`, has none.",
       if (looks_like_n) {
         paste0("\n  If this is the number of rows, write `N = ",
                format(val), "`: N is supplied by name.")
       },
       call. = FALSE)
}

# Level constructor helpers ----
# Each public function returns a fabricatr_level object.
# fabricate() detects these via inherits() and dispatches to execute_*_level().

new_level <- function(type, ...) {
  structure(list(type = type, ...), class = "fabricatr_level")
}

# Every ID column in the package is built here. Zero-padded character, as in
# fabricatr: an ID is a label, not a quantity, and a character ID cannot be
# swept into a regression as a linear term. The padding width is set by the
# number of units at the level, so character sort order matches numeric order.
make_ids <- function(n) {
  formatC(seq_len(n), width = nchar(n), flag = "0")
}

# fabricatr's `nest =` and `by =` arrive inside `...` and would otherwise be
# captured as ordinary column expressions. They are accepted and deprecated in
# R/deprecated.R, which warns with the call the author should have written.

# A column expression inside a nested level has to fill the level. Any vector
# whose length divides the level evenly is recycled, as in fabricatr, which is
# what makes `task = 1:3` number the tasks within every parent. Recycling is
# safe here only because `N` is the level's total row count: an expression
# written against `N` already returns one value per row, so the short vectors
# that reach this point are the ones the author wrote out deliberately.
recycle_to_level <- function(val, n_total, col_nm) {
  n <- length(val)
  if (n == n_total) return(val)
  if (n > 0L && n_total %% n == 0L) return(rep(val, length.out = n_total))
  stop("In a nested level, `", col_nm, "` returned ", n, " values, which ",
       "does not fill the level's ", n_total, " rows. Return one value per ",
       "row, or a vector whose length divides ", n_total, " evenly.",
       call. = FALSE)
}

# add_level -------------------------------------------------------------------

#' Add a hierarchical level
#'
#' Creates N rows and registers the result as the new current data frame.
#' Subsequent calls to \code{nest_level} will fan out from these rows.
#' For independent (non-nested) levels used in \code{cross_levels} or
#' \code{link_levels}, use \code{declare_level}.
#'
#' @param N Number of rows to create.
#' @param ... Column expressions evaluated sequentially. \code{N} is available
#'   as a scalar integer.
#'
#' @return A \code{fabricatr_level} object (used inside \code{fabricate}).
#'
#' @examples
#' fabricate(
#'   villages = add_level(N = 20, income = rnorm(N)),
#'   citizens = nest_level(N = 5, Y = income + rnorm(N))
#' )
#'
#' @export
add_level <- function(N, ...) {
  legacy <- absorb_legacy_nest(rlang::enquos(...), sys.call(), "add")
  new_level(legacy$type, N = N, dots = legacy$dots)
}

# declare_level ---------------------------------------------------------------

#' Declare an independent level for cross-classification
#'
#' Creates N rows as a standalone data frame registered for use by
#' \code{cross_levels} or \code{link_levels}. Unlike \code{add_level}, a
#' \code{declare_level} call does not nest into any existing hierarchy — it
#' starts fresh. Use this when you need two independent populations to cross
#' (e.g., countries and years for panel data).
#'
#' @param N Number of rows to create.
#' @param ... Column expressions evaluated sequentially. \code{N} is available
#'   as a scalar integer.
#'
#' @return A \code{fabricatr_level} object (used inside \code{fabricate}).
#'
#' @examples
#' fabricate(
#'   countries = declare_level(N = 20, gdp = runif(N, 1, 10)),
#'   years     = declare_level(N = 10, shock = runif(N, 1, 5)),
#'   obs       = cross_levels(
#'     .by = c("countries", "years"),
#'     GDP_it = gdp + shock
#'   )
#' )
#'
#' @export
declare_level <- function(N, ...) {
  legacy <- absorb_legacy_nest(rlang::enquos(...), sys.call(), "declare")
  new_level(legacy$type, N = N, dots = legacy$dots)
}

# nest_level ------------------------------------------------------------------

#' Nest a level within the current hierarchy
#'
#' For each row in the current data frame, creates N child rows. Parent columns
#' are replicated across children. \code{N} may be a scalar (same count for
#' every parent) or a vector of length \code{nrow} of the parent (variable
#' children per parent).
#'
#' @param N Rows per parent. Scalar or per-parent vector.
#' @param ... Column expressions. Note that the \code{N} visible inside these
#'   expressions is the total number of rows the level creates, not the
#'   per-parent count in the \code{N} argument: nesting 5 citizens in each of
#'   20 villages makes \code{N} equal to 100 here. The behaviour matches
#'   fabricatr, and it is what makes \code{rnorm(N)} draw independently for
#'   every village rather than drawing five values and reusing them.
#'
#' @return A \code{fabricatr_level} object (used inside \code{fabricate}).
#'
#' @examples
#' fabricate(
#'   villages = add_level(N = 20, v_income = rnorm(N)),
#'   citizens = nest_level(N = 5, income = v_income + rnorm(N))
#' )
#'
#' @export
nest_level <- function(N, ...) {
  dots <- rlang::enquos(...)
  new_level("nest", N = rlang::enquo(N), dots = dots)
}

# cross_levels ----------------------------------------------------------------

#' Create a full Cartesian product of declared levels
#'
#' Produces all combinations of the specified levels (equivalent to SQL
#' CROSS JOIN). Use \code{link_levels} to sample N rows from the product with
#' an optional correlation structure.
#'
#' @param .by Character vector of level names to cross (must have been created
#'   by \code{add_level} or \code{declare_level} in the same \code{fabricate}
#'   call).
#' @param ... Additional column expressions evaluated after crossing.
#'
#' @return A \code{fabricatr_level} object (used inside \code{fabricate}).
#'
#' @examples
#' fabricate(
#'   countries = declare_level(N = 10, gdp = runif(N, 1, 10)),
#'   years     = declare_level(N = 5, shock = runif(N, 0, 1)),
#'   obs       = cross_levels(.by = c("countries", "years"), Y = gdp + shock)
#' )
#'
#' @export
cross_levels <- function(.by, ...) {
  legacy <- absorb_legacy_by(rlang::enquos(...), sys.call())
  by <- if (is.null(legacy$by)) .by else legacy$by
  new_level("cross", by = by, dots = legacy$dots)
}

# link_levels -----------------------------------------------------------------

#' Sample N rows from a Cartesian product with optional correlation
#'
#' Draws N rows from the cross-product of the specified levels. When \code{rho}
#' or \code{sigma} is non-zero, row assignments are correlated via a Gaussian
#' copula so that units with high values on one level's variable tend to be
#' paired with units with high values on the other level's variable.
#'
#' @param N Number of rows to sample from the product.
#' @param .by Character vector of exactly two level names.
#' @param rho Scalar Spearman rank correlation between the two levels' row
#'   assignments (default 0 = independent). Ignored if \code{sigma} is
#'   provided.
#' @param sigma Square correlation matrix (dimension = \code{length(.by)}).
#' @param ... Additional column expressions evaluated after linking.
#'
#' @return A \code{fabricatr_level} object (used inside \code{fabricate}).
#'
#' @examples
#' fabricate(
#'   primary   = declare_level(N = 20, p_quality = runif(N, 1, 10)),
#'   secondary = declare_level(N = 15, s_quality = runif(N, 1, 10)),
#'   students  = link_levels(
#'     N = 200, .by = c("primary", "secondary"), rho = 0.5,
#'     score = p_quality + s_quality + rnorm(N)
#'   )
#' )
#'
#' @export
link_levels <- function(N, .by, rho = 0, sigma = NULL, ...) {
  legacy <- absorb_legacy_by(rlang::enquos(...), sys.call())
  by <- if (is.null(legacy$by)) .by else legacy$by
  new_level("link", N = N, by = by, rho = rho, sigma = sigma,
            dots = legacy$dots)
}

# modify_level ----------------------------------------------------------------

#' Modify columns of the current level, optionally within groups
#'
#' Equivalent to \code{dplyr::mutate}, with an optional \code{.by} argument
#' for split-apply-combine on a grouping column. Replaces fabricatr's
#' \code{modify_level}.
#'
#' @param ... Column expressions to add or overwrite.
#' @param .by Optional character string: column name to group by before
#'   evaluating expressions.
#'
#' @return A \code{fabricatr_level} object (used inside \code{fabricate}).
#'
#' @examples
#' fabricate(
#'   N = 50,
#'   cluster = sample(1:5, N, replace = TRUE),
#'   Y = rnorm(N),
#'   modify_level(cluster_mean = mean(Y), .by = "cluster")
#' )
#'
#' @export
modify_level <- function(..., .by = NULL) {
  legacy <- absorb_legacy_by(rlang::enquos(...), sys.call())
  new_level("modify", dots = legacy$dots, by = legacy$by %||% .by)
}

# execute_* functions ---------------------------------------------------------
# All execute functions operate on and return plain named lists.
# fabricate_impl converts to tibble once at the very end via list_to_df().
# Direct list indexing (v[idx]) is far cheaper than tibble row subsetting.

execute_add_level <- function(level, nm) {
  N_val <- as.integer(level$N)
  base <- list()
  if (nchar(nm) > 0) base[[nm]] <- make_ids(N_val)
  lst <- eval_dots_into_list(level$dots, base, inner_N = N_val)
  lst[["N"]] <- NULL
  lst
}

# Signature updated: takes a plain list + N_inject scalar (not a tibble).
execute_nest_level <- function(level, lst, N_inject, nm) {
  n_parent <- if (length(lst) > 0L) length(lst[[1L]]) else 0L
  if (n_parent == 0L) {
    stop("nest_level() requires an existing level to nest within. ",
         "Use add_level() first to create the top level.")
  }

  N_val <- rlang::eval_tidy(level$N, data = lst)

  if (length(N_val) == 1L) {
    idx     <- rep(seq_len(n_parent), each = N_val)
    inner_N <- N_val
  } else {
    if (length(N_val) != n_parent) {
      stop("In nest_level(), N must be a scalar or a vector of length nrow(parent).")
    }
    idx     <- rep(seq_len(n_parent), times = N_val)
    inner_N <- rep(N_val, times = N_val)
  }

  N_total  <- length(idx)
  # Direct vector indexing — no data.frame/tibble overhead
  expanded <- lapply(lst, function(v) v[idx])
  # `N` at a nested level is the total number of rows the level creates, as in
  # fabricatr. Setting it to the per-parent group size instead would make a
  # stochastic expression return one group's worth of values, and the only way
  # to fill the level would be to repeat them, handing every parent the
  # identical draw.
  expanded[["N"]] <- N_total
  if (nchar(nm) > 0L) expanded[[nm]] <- make_ids(N_total)

  for (i in seq_along(level$dots)) {
    col_nm <- names(level$dots)[[i]]
    val    <- rlang::eval_tidy(level$dots[[i]], data = expanded)

    if (nchar(col_nm) > 0L) {
      expanded <- store_column(expanded, col_nm, val,
                               function(v, cn) recycle_to_level(v, N_total, cn))
    } else if (is.data.frame(val)) {
      for (j in seq_along(val)) {
        expanded[[names(val)[[j]]]] <-
          recycle_to_level(val[[j]], N_total, names(val)[[j]])
      }
    }
  }

  expanded[["N"]] <- NULL
  expanded
}

# Pure-list Cartesian product (avoids data.frame construction overhead).
cross_join_lists <- function(a, b) {
  na <- length(a[[1L]])
  nb <- length(b[[1L]])
  a_exp <- lapply(a, rep, times = nb)
  b_exp <- lapply(b, rep, each  = na)
  c(a_exp, b_exp)
}

execute_cross_level <- function(level, level_registry, nm) {
  missing <- setdiff(level$by, names(level_registry))
  if (length(missing) > 0L) {
    stop("cross_levels: levels not found in registry: ",
         paste(missing, collapse = ", "),
         ". Did you use add_level() or declare_level() to create them?")
  }
  if (length(level$by) < 2L) stop("cross_levels: specify at least 2 levels in .by.")

  lsts  <- level_registry[level$by]
  base  <- Reduce(cross_join_lists, lsts)
  base[["N"]] <- NULL
  N_val <- length(base[[1L]])
  if (nchar(nm) > 0L) base[[nm]] <- make_ids(N_val)

  lst <- eval_dots_into_list(level$dots, base, inner_N = N_val)
  lst[["N"]] <- NULL
  lst
}

execute_link_level <- function(level, level_registry, nm) {
  missing <- setdiff(level$by, names(level_registry))
  if (length(missing) > 0L) {
    stop("link_levels: levels not found in registry: ",
         paste(missing, collapse = ", "))
  }
  lsts <- level_registry[level$by]
  N    <- as.integer(level$N)

  indices <- joint_draw_ecdf(
    data_list = lapply(lsts, function(d) seq_len(length(d[[1L]]))),
    N = N, sigma = level$sigma, rho = level$rho
  )

  base <- lapply(lsts[[1L]], function(v) v[indices[[1L]]])
  for (i in seq_along(lsts)[-1L]) {
    extra <- lapply(lsts[[i]], function(v) v[indices[[i]]])
    base  <- c(base, extra)
  }
  base[["N"]] <- NULL
  if (nchar(nm) > 0L) base[[nm]] <- make_ids(N)

  lst <- eval_dots_into_list(level$dots, base, inner_N = N)
  lst[["N"]] <- NULL
  lst
}

execute_modify_level <- function(level, lst, N_inject) {
  if (is.null(level$by)) {
    out <- eval_dots_into_list(level$dots, lst, inner_N = N_inject)
    out[["N"]] <- NULL
    out
  } else {
    by_col <- level$by
    grp_vec <- lst[[by_col]]
    groups  <- split(seq_along(grp_vec), grp_vec)
    orig_order <- order(unlist(groups, use.names = FALSE))
    slices <- purrr::map(groups, function(idx) {
      n_sl <- length(idx)
      sl   <- lapply(lst, function(v) v[idx])
      out  <- eval_dots_into_list(level$dots, sl, inner_N = n_sl)
      out[["N"]] <- NULL
      # Recycle scalars to slice length (mirrors tibble's behaviour)
      lapply(out, function(v) if (length(v) == 1L && n_sl > 1L) rep(v, n_sl) else v)
    })
    # Bind list-of-lists by column then restore original row order
    bound <- lapply(names(slices[[1L]]), function(nm) {
      unlist(lapply(slices, `[[`, nm), use.names = FALSE)
    })
    names(bound) <- names(slices[[1L]])
    lapply(bound, function(v) v[orig_order])
  }
}

# Gaussian copula for link_levels ---------------------------------------------

# chol(pivot = TRUE) accepts a matrix that is not positive semi-definite,
# warns, and returns a decomposition that produces draws with none of the
# requested correlation structure. Reject those inputs here instead, matching
# the checks fabricatr makes.
check_sigma <- function(sigma, ndim) {
  if (!is.matrix(sigma) || !is.numeric(sigma) ||
      nrow(sigma) != ndim || ncol(sigma) != ndim) {
    stop("`sigma` must be a numeric matrix with one row and one column per ",
         "level in `.by` (", ndim, "x", ndim, ").", call. = FALSE)
  }
  if (any(diag(sigma) != 1)) {
    stop("The diagonal of `sigma` must be all 1s.", call. = FALSE)
  }
  if (!isSymmetric(sigma)) {
    stop("`sigma` must be symmetric.", call. = FALSE)
  }
  if (any(sigma < -1 | sigma > 1)) {
    stop("Every entry of `sigma` must lie between -1 and 1.", call. = FALSE)
  }
  eigenvalues <- eigen(sigma, symmetric = TRUE, only.values = TRUE)$values
  if (any(eigenvalues < -1e-8)) {
    stop("`sigma` must be positive semi-definite. Not every set of pairwise ",
         "correlations is jointly attainable: strong negative correlations ",
         "among three or more levels are a common way to ask for an ",
         "impossible one.", call. = FALSE)
  }
  invisible(NULL)
}

joint_draw_ecdf <- function(data_list, N, sigma = NULL, rho = 0) {
  ndim <- length(data_list)

  if (!is.numeric(N) || length(N) != 1L || is.na(N) || N <= 0) {
    stop("`N` in link_levels() must be a single positive number.", call. = FALSE)
  }

  if (is.null(sigma)) {
    if (!is.numeric(rho) || length(rho) != 1L) {
      stop("`rho` in link_levels() must be a single number.", call. = FALSE)
    }
    if (rho == 0) {
      return(lapply(data_list, function(v) sample.int(length(v), N, replace = TRUE)))
    }
    if (ndim > 2L && rho < 0) {
      stop("With three or more levels, a single negative `rho` cannot describe ",
           "a positive semi-definite correlation matrix. Supply `sigma` ",
           "directly if you need negative correlations.", call. = FALSE)
    }
    sigma <- matrix(rho, nrow = ndim, ncol = ndim)
    diag(sigma) <- 1
  }

  check_sigma(sigma, ndim)

  use_mvnfast <- requireNamespace("mvnfast", quietly = TRUE)
  mu <- rep(0, ndim)

  if (use_mvnfast) {
    corr_sn <- mvnfast::rmvn(N, mu, sigma)
  } else {
    R <- chol(sigma, pivot = TRUE)
    R <- R[, order(attr(R, "pivot"))]
    corr_sn <- matrix(stats::rnorm(N * ndim), nrow = N) %*% R
  }

  quantiles <- stats::pnorm(corr_sn)

  lapply(seq_len(ndim), function(j) {
    v <- data_list[[j]]
    ordered_idx <- pmax(1L, round(quantiles[, j] * length(v)))
    order(v)[ordered_idx]
  })
}

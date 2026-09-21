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
#' @return A \code{fabricatr_level} object, meaningful only as a named
#'   argument to \code{fabricate}. It contributes \code{N} rows, an ID column
#'   named after the argument it is assigned to, and one column per expression
#'   in \code{...}. Any level nested below it repeats these rows.
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
  if (missing(N)) stop_missing_n("add_level()", legacy$dots)
  new_level(legacy$type, N = N, dots = legacy$dots)
}

# declare_level ---------------------------------------------------------------

#' Declare an independent level for cross-classification
#'
#' Creates N rows as a standalone data frame registered for use by
#' \code{cross_levels} or \code{link_levels}. Unlike \code{add_level}, a
#' \code{declare_level} call does not nest into any existing hierarchy: it
#' starts fresh. Use this when you need two independent populations to cross
#' (e.g., countries and years for panel data).
#'
#' @param N Number of rows to create.
#' @param ... Column expressions evaluated sequentially. \code{N} is available
#'   as a scalar integer.
#'
#' @return A \code{fabricatr_level} object, meaningful only as a named
#'   argument to \code{fabricate}. It builds \code{N} rows and an ID column
#'   named after the argument it is assigned to, and holds them aside: the rows
#'   reach the fabricated frame only when a later \code{cross_levels} or
#'   \code{link_levels} names this level in \code{.by}. Declaring a level that
#'   nothing crosses or links contributes nothing to the result.
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
  if (missing(N)) stop_missing_n("declare_level()", legacy$dots)
  new_level(legacy$type, N = N, dots = legacy$dots)
}

# import_level ----------------------------------------------------------------

#' Import an existing data frame as a level
#'
#' Brings a data frame into a \code{fabricate} call as a level of its own, so
#' that real data can be crossed or linked with other real data.
#' \code{fabricate(data = )} starts from one frame and builds down from it;
#' \code{import_level} makes each frame a named level, which is the thing
#' \code{cross_levels} and \code{link_levels} work on.
#'
#' @param data A data frame. Its rows are the level's units.
#' @param ... Column expressions evaluated against the imported columns, as in
#'   any other level. \code{N} is the number of rows imported.
#' @param .id Optional string naming the column of \code{data} that already
#'   identifies its rows. Left off, a column named after the level is used when
#'   the data has one, the way a join matches on a shared name, and otherwise
#'   fabricatr numbers the rows itself.
#'
#' @section Which column is the ID:
#' A level's ID column is named after the level, and \code{import_level} only
#' decides what goes in it. Three cases, in order:
#'
#' \itemize{
#'   \item \code{.id = "pid"} uses that column of the imported data, renamed
#'     to the level's name. It has to exist, and its values have to be distinct
#'     and non-missing, because a level's rows are its units.
#'   \item Left off, a column already named after the level is used as it
#'     stands: \code{individuals = import_level(df)} where \code{df} has an
#'     \code{individuals} column keeps that column rather than putting a
#'     second ID beside it.
#'   \item Otherwise the rows are numbered, as \code{add_level} numbers them.
#' }
#'
#' A column called \code{N} is refused. \code{N} is the number of rows the
#' level is building in every expression it evaluates, so the two cannot both
#' be in view; rename the column before importing it, or use
#' \code{fabricate(data = )}, which evaluates nothing at a level and keeps it.
#'
#' An imported ID keeps its own type and values, where a fabricated one is a
#' zero-padded character string. That is the point of importing it: the column
#' still matches the same key in the data it came from. It also means an
#' integer key stays an integer, so a model formula reading it will treat it
#' as a quantity unless you make it a factor.
#'
#' @return A \code{fabricatr_level} object, meaningful only as a named
#'   argument to \code{fabricate}. It contributes the imported rows, an ID
#'   column named after the argument it is assigned to, and one column per
#'   expression in \code{...}. Like \code{declare_level}, it stands alone
#'   rather than nesting into the frame in hand, so it can be named in a later
#'   \code{cross_levels} or \code{link_levels}.
#'
#' @examples
#' individuals <- data.frame(individuals = c("ann", "bob", "cyd"),
#'                           ind_shock = c(-0.4, 0.1, 0.8))
#' periods <- data.frame(year = 2020:2022, period_shock = c(0.2, -0.1, 0.3))
#'
#' # Each frame keeps its own key: `individuals` by name, `year` by `.id`
#' fabricate(
#'   individuals = import_level(individuals),
#'   period      = import_level(periods, .id = "year"),
#'   obs         = cross_levels(
#'     .by = c("individuals", "period"),
#'     Y = ind_shock + period_shock + rnorm(N)
#'   )
#' )
#'
#' # Imported units with fabricated units nested inside them
#' fabricate(
#'   villages = import_level(data.frame(villages = c("v1", "v2"),
#'                                      v_income = c(10, 12))),
#'   citizens = nest_level(N = 3, income = v_income + rnorm(N))
#' )
#'
#' @export
import_level <- function(data, ..., .id = NULL) {
  new_level("import", data = data, id = .id, dots = rlang::enquos(...))
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
#'   fabricatr 1.x, and it is what makes \code{rnorm(N)} draw independently for
#'   every village rather than drawing five values and reusing them.
#'
#' @return A \code{fabricatr_level} object, meaningful only as a named
#'   argument to \code{fabricate}. It replaces each row of the current frame
#'   with \code{N} child rows, repeating the parent's columns down them, and
#'   adds an ID column named after the argument it is assigned to. That ID is
#'   unique across the whole frame rather than restarting within each parent.
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
  if (missing(N)) stop_missing_n("nest_level()", dots)
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
#' @return A \code{fabricatr_level} object, meaningful only as a named
#'   argument to \code{fabricate}. It contributes one row per combination of
#'   the levels named in \code{.by}, so the frame has as many rows as their
#'   sizes multiplied together, carrying every column of those levels plus an
#'   ID column of its own. The first level in \code{.by} varies fastest.
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
#' The correlated draw takes one code path, so a given seed produces the same
#' data on every machine. fabricatr 1.x switches to \code{mvnfast::rmvn()} when
#' that package is installed, and passes it a core count, so its numbers move
#' when either changes. This is the second and last place where 2.0
#' can give different numbers than 1.x, and it only differs when
#' \code{mvnfast} is installed.
#'
#' @param N Number of rows to sample from the product.
#' @param .by Character vector of level names, usually two.
#' @param ... Additional column expressions evaluated after linking.
#' @param rho Scalar correlation of the Gaussian copula that links the levels'
#'   row assignments (default 0 = independent). It is not the realized rank
#'   correlation, which is smaller by the copula's own relation,
#'   \eqn{(6/\pi)\arcsin(\rho/2)}: \code{rho = 0.7} lands at 0.68 and
#'   \code{rho = 0.3} at 0.283. Ignored if \code{sigma} is provided. Comes
#'   after \code{...}, so a column called \code{r} or \code{s} is a column
#'   and not a partial match.
#' @param sigma Square correlation matrix (dimension = \code{length(.by)}).
#'
#' @return A \code{fabricatr_level} object, meaningful only as a named
#'   argument to \code{fabricate}. It contributes \code{N} rows drawn from the
#'   same product \code{cross_levels} would build, with replacement, so a
#'   combination can appear more than once and others not at all, and \code{N}
#'   may exceed the size of the product. Columns are as for
#'   \code{cross_levels}.
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
link_levels <- function(N, .by, ..., rho = 0, sigma = NULL) {
  legacy <- absorb_legacy_by(rlang::enquos(...), sys.call())
  by <- if (is.null(legacy$by)) .by else legacy$by
  new_level("link", N = N, by = by, rho = legacy$rho %||% rho, sigma = sigma,
            dots = legacy$dots)
}

# modify_level ----------------------------------------------------------------

#' Modify columns of an existing level, or of the rows in hand
#'
#' Adds or overwrites columns, as \code{dplyr::mutate} does. Named after a
#' level that already exists, it works at that level: the expressions are
#' evaluated once per unit of the level, \code{N} is the number of units, and
#' the result is written back to every row of each unit. Left unnamed, it
#' works on the rows in hand, with an optional \code{.by} for
#' split-apply-combine on a grouping column.
#'
#' @param ... Column expressions to add or overwrite.
#' @param .by Optional character string: column name to group by before
#'   evaluating expressions.
#'
#' @section Which columns a level can see:
#' Inside \code{regions = modify_level(...)}, the columns in view are the ones
#' that take a single value per region: those built at the regions level or
#' above it. A column built at a level nested inside regions varies within a
#' region, so it is out of view, as it is in fabricatr 1.x. To summarise such a
#' column per region, leave the level name off and group instead:
#' \code{modify_level(mean_b = mean(b), .by = "regions")}.
#'
#' @return A \code{fabricatr_level} object, meaningful only as a named or
#'   unnamed argument to \code{fabricate}. It adds or overwrites columns and
#'   changes no rows: the frame it returns has the same number of rows as the
#'   one it received.
#'
#' @examples
#' # At a named level: one draw per region, written to every city in it
#' fabricate(
#'   regions = add_level(N = 3, a = 1:3),
#'   cities  = add_level(N = 2, b = rnorm(N)),
#'   regions = modify_level(z = rnorm(N), a2 = a * 2)
#' )
#'
#' # On the rows in hand, grouped
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
  by <- legacy$by %||% .by
  # `.by` is an ordinary argument, so a bare column name evaluates to the
  # column itself and arrives here as a vector. `cross_levels()` and
  # `link_levels()` already name the argument when their `.by` is wrong; this
  # one reached `lst[[by]]` and failed inside the subscript.
  if (!is.null(by) && (!is.character(by) || length(by) != 1L || is.na(by))) {
    stop("`modify_level(.by = )` takes one column name, written as a string, ",
         "as in `modify_level(cluster_mean = mean(Y), .by = \"cluster\")`.",
         call. = FALSE)
  }
  new_level("modify", dots = legacy$dots, by = by)
}

# execute_* functions ---------------------------------------------------------
# All execute functions operate on and return plain named lists.
# fabricate_impl converts to tibble once at the very end via list_to_df().
# Direct list indexing (v[idx]) is far cheaper than tibble row subsetting.

execute_add_level <- function(level, nm) {
  N_val <- validate_n(level$N, "add_level()")
  base <- list()
  if (nchar(nm) > 0) base[[nm]] <- make_ids(N_val)
  lst <- eval_dots_into_list(level$dots, base, inner_N = N_val)
  lst[["N"]] <- NULL
  lst
}

# An imported level's ID is a column of the data rather than something
# fabricatr makes up, which is the whole of fabricatr#165: two real data sets
# can only be crossed if each one's own key survives the crossing. The ID
# column is named after the level, as every other level's is, so
# `modify_level()` named after the level, `cross_levels(.by = )`, and the
# disjointness check all read it without knowing where it came from.
execute_import_level <- function(level, nm) {
  data <- level$data
  if (!is.data.frame(data)) {
    stop("import_level() builds a level out of a data frame, and was given ",
         "an object of class ", class(data)[1L], ".", call. = FALSE)
  }
  n <- nrow(data)
  if (n == 0L) {
    stop("import_level() was given a data frame with no rows. A level's rows ",
         "are its units, so it needs at least one.", call. = FALSE)
  }
  lst <- as.list(tibble::as_tibble(data))
  if ("N" %in% names(lst)) {
    # `N` is the row count in every expression a level evaluates, and the
    # level's own bookkeeping drops it on the way out, so an imported column
    # of that name would leave the frame silently. Refusing is the only
    # reading that cannot lose data. `fabricate(data = )` keeps such a column,
    # because it evaluates nothing at a level and has nothing to drop.
    stop("The imported data has a column called `N`, and `N` is the number of ",
         "rows the level is building in every expression, so the two cannot ",
         "both be in view. Rename the column before importing it. If you only ",
         "need this one data frame, `fabricate(data = )` keeps a column ",
         "called `N`.", call. = FALSE)
  }

  id_col <- resolve_import_id(level$id, names(lst), nm)
  if (is.null(id_col)) {
    if (nzchar(nm)) lst <- c(stats::setNames(list(make_ids(n)), nm), lst)
  } else {
    check_import_id(lst[[id_col]], id_col, nm, explicit = !is.null(level$id))
    if (id_col != nm && nm %in% names(lst)) {
      stop("`.id = \"", id_col, "\"` in import_level() renames that column to ",
           "`", nm, "`, the level's name, and the imported data already has a ",
           "column called `", nm, "`. Rename one of them, or name the level ",
           "something else.", call. = FALSE)
    }
    lst <- c(stats::setNames(lst[id_col], nm),
             lst[setdiff(names(lst), id_col)])
  }

  lst <- eval_dots_into_list(level$dots, lst, inner_N = n)
  lst[["N"]] <- NULL
  lst
}

# Returns the name of the column to use as the level's ID, or NULL to number
# the rows. The unnamed default is a name match, which is how a join finds the
# column two frames share, and is what makes the common case,
# `individuals = import_level(individuals_data)`, need no argument at all.
resolve_import_id <- function(id, cols, nm) {
  if (is.null(id)) {
    return(if (nzchar(nm) && nm %in% cols) nm else NULL)
  }
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    stop("`.id` in import_level() names one column of the imported data, as ",
         "a string.", call. = FALSE)
  }
  if (!id %in% cols) {
    stop("`.id = \"", id, "\"` in import_level(): the imported data has no ",
         "column called `", id, "`. It has ",
         paste0("`", cols, "`", collapse = ", "), ".", call. = FALSE)
  }
  if (!nzchar(nm)) {
    stop("`.id` in import_level() renames the ID column after the level, and ",
         "this import_level() has no level name. Write ",
         "`<level> = import_level(data, .id = \"", id, "\")`.", call. = FALSE)
  }
  id
}

check_import_id <- function(v, id_col, nm, explicit) {
  hint <- if (explicit) {
    paste0("Point `.id` at a column whose values are distinct, or leave it ",
           "off to have fabricatr number the rows.")
  } else {
    paste0("`", id_col, "` was used because it is named after the level. ",
           "Set `.id` to the column that identifies a ", nm, ", or rename ",
           "this one.")
  }
  if (anyNA(v)) {
    n_na <- sum(is.na(v))
    stop("`", id_col, "` cannot be the ID of level `", nm, "`: ", n_na,
         " of its ", length(v), " values ", if (n_na == 1L) "is" else "are",
         " missing, and a missing ID identifies nothing. ", hint,
         call. = FALSE)
  }
  dup <- unique(v[duplicated(v)])
  if (length(dup) > 0L) {
    shown <- paste0("`", dup[seq_len(min(3L, length(dup)))], "`",
                    collapse = ", ")
    stop("`", id_col, "` cannot be the ID of level `", nm, "`: ", length(dup),
         " of its values ", if (length(dup) == 1L) "appears" else "appear",
         " more than once (", shown,
         if (length(dup) > 3L) ", and others", "). A level's rows are its ",
         "units, so their IDs have to be distinct. ", hint, call. = FALSE)
  }
  invisible(NULL)
}

# Signature updated: takes a plain list + N_inject scalar (not a tibble).
execute_nest_level <- function(level, lst, N_inject, nm) {
  n_parent <- if (length(lst) > 0L) length(lst[[1L]]) else 0L
  if (n_parent == 0L) {
    stop("nest_level() requires an existing level to nest within. ",
         "Use add_level() first to create the top level.")
  }

  N_val <- validate_n(rlang::eval_tidy(level$N, data = lst),
                      "nest_level()", scalar = FALSE)

  if (length(N_val) == 1L) {
    idx <- rep(seq_len(n_parent), each = N_val)
  } else {
    if (length(N_val) != n_parent) {
      stop("In nest_level(), N must be a scalar or a vector of length nrow(parent).")
    }
    idx <- rep(seq_len(n_parent), times = N_val)
  }

  N_total  <- length(idx)
  # Direct vector indexing, with no data.frame/tibble overhead
  expanded <- lapply(lst, function(v) v[idx])
  # `N` at a nested level is the total number of rows the level creates, as in
  # fabricatr. Setting it to the per-parent group size instead would make a
  # stochastic expression return one group's worth of values, and the only way
  # to fill the level would be to repeat them, handing every parent the
  # identical draw.
  expanded[["N"]] <- N_total
  if (nchar(nm) > 0L) expanded[[nm]] <- make_ids(N_total)

  m <- level_mask(expanded, N_total)

  for (i in seq_along(level$dots)) {
    col_nm <- names(level$dots)[[i]]
    val    <- mask_eval(m, level$dots[[i]])

    if (nchar(col_nm) > 0L) {
      expanded <- store_column(expanded, col_nm, val,
                               function(v, cn) recycle_to_level(v, N_total, cn),
                               m)
    } else if (is.data.frame(val)) {
      for (j in seq_along(val)) {
        v <- recycle_to_level(val[[j]], N_total, names(val)[[j]])
        expanded[[names(val)[[j]]]] <- v
        mask_bind(m, names(val)[[j]], v)
      }
    } else {
      stop_unnamed_expression(i, level$dots[[i]], val, "nest_level()")
    }
  }

  expanded[["N"]] <- NULL
  expanded
}

# Pure-list Cartesian product (avoids data.frame construction overhead).
# A level built with add_level() is nested inside the level before it, and its
# registry entry carries that ancestor's columns. Crossing or linking the two
# would concatenate the shared columns into a duplicate name, which surfaces
# much later as a name-uniqueness failure that says nothing about the cause.
# 1.0.2 refuses the same combination, naming the ambiguous level.
check_levels_disjoint <- function(lsts, fn) {
  shared <- setdiff(names(which(table(unlist(lapply(lsts, names))) > 1L)), "N")
  if (length(shared) > 0L) {
    stop(fn, ": the levels in `.by` both carry ",
         paste0("`", shared, "`", collapse = ", "),
         ", so joining them would produce duplicate columns. A level created ",
         "with add_level() is nested inside the one before it and carries its ",
         "columns; declare_level() creates a level that stands alone.",
         call. = FALSE)
  }
  invisible(NULL)
}

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
  check_levels_disjoint(lsts, "cross_levels")
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
  check_levels_disjoint(lsts, "link_levels")
  N    <- validate_n(level$N, "link_levels()")

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

# `regions = modify_level()` after a `cities` level has been nested inside it
# evaluates once per region, not once per city. The units are the distinct
# values of the level's ID column, `N` is their number, and the columns in
# view are the ones that take a single value per unit: those built at that
# level or above it. A column that varies within a unit is out of view, as in
# fabricatr 1.x, where the same call could see nothing below the level it
# named. Each new column is evaluated per unit and written back to every row
# of the unit. Returns the frame and the level's own rows, which is what a
# later `cross_levels()` or `link_levels()` on this level should see.
modify_existing_level <- function(level, lst, nm) {
  id <- lst[[nm]]
  units <- unique(id)
  unit_of_row <- match(id, units)
  first_row <- match(units, id)
  n_units <- length(units)

  per_unit <- lapply(lst, function(v) v[first_row])
  in_view <- vapply(names(lst), function(cn) {
    identical(per_unit[[cn]][unit_of_row], lst[[cn]])
  }, logical(1))

  out <- tryCatch(
    eval_dots_into_list(level$dots, per_unit[in_view], inner_N = n_units),
    error = function(e) {
      missing <- regmatches(conditionMessage(e),
                            regexec("^object '([^']+)' not found$",
                                    conditionMessage(e)))[[1L]][2L]
      if (is.na(missing) || !missing %in% names(lst)[!in_view]) stop(e)
      stop("`", missing, "` is out of view inside `", nm, " = modify_level()`: ",
           "it varies within ", nm, ", so it belongs to a level nested inside ",
           nm, ". To summarise it per ", nm, ", leave the level name off and ",
           "write `modify_level(..., .by = \"", nm, "\")`.", call. = FALSE)
    }
  )
  out[["N"]] <- NULL

  for (cn in names(out)) {
    v <- out[[cn]]
    if (length(v) != n_units) {
      stop("In `", nm, " = modify_level()`, `", cn, "` returned ", length(v),
           " values for ", n_units, " ", nm, ". Return one value per unit of ",
           "the level.", call. = FALSE)
    }
    lst[[cn]] <- v[unit_of_row]
  }
  list(data = lst, level = out)
}

execute_modify_level <- function(level, lst, N_inject) {
  if (is.null(level$by)) {
    out <- eval_dots_into_list(level$dots, lst, inner_N = N_inject)
    out[["N"]] <- NULL
    out
  } else {
    by_col <- level$by
    if (!by_col %in% names(lst)) {
      stop("`modify_level(.by = \"", by_col, "\")`: no column named `",
           by_col, "` is in view. The columns here are ",
           paste0("`", names(lst), "`", collapse = ", "), ".", call. = FALSE)
    }
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

  # One draw path, always. fabricatr switches to mvnfast::rmvn() when that
  # package is installed, and the two consume the RNG differently, so the same
  # seed gives different data on a machine that happens to have it. fabricatr
  # also passes `ncores = getOption("mc.cores", 2L)`, which makes the draw
  # depend on the core count as well. For a package whose job is simulation
  # that is the wrong trade: measured at three levels, mvnfast saves 19
  # milliseconds on a million rows and nothing at all at the sizes anyone
  # links, so the branch bought speed nobody can feel with reproducibility
  # everybody needs.
  R <- chol(sigma, pivot = TRUE)
  R <- R[, order(attr(R, "pivot"))]
  corr_sn <- matrix(stats::rnorm(N * ndim), nrow = N) %*% R

  quantiles <- stats::pnorm(corr_sn)

  lapply(seq_len(ndim), function(j) {
    v <- data_list[[j]]
    ordered_idx <- pmax(1L, round(quantiles[, j] * length(v)))
    order(v)[ordered_idx]
  })
}

# fabricatr#165 was filed on `add_level(data = df, ...)`, which has never
# imported a data frame: 1.x let `N` resolve to NULL and failed on that, and
# here `data` is an ordinary column expression that would be stored as a
# column called `data`. Either way the call lands on the missing `N`, so that
# is where import_level() gets named. The argument is only evaluated when it
# is a bare name, the shape the issue reports and the one shape that can be
# looked at here without running an author's expression an extra time.
stop_missing_n <- function(fn, dots) {
  what <- if (fn == "nest_level()") {
    "the number of rows to create for each row in hand"
  } else {
    "the number of rows the level builds"
  }
  msg <- paste0(fn, " needs `N`, ", what, ".")
  if ("data" %in% names(dots)) {
    quo <- dots[["data"]]
    if (rlang::is_symbol(rlang::quo_get_expr(quo))) {
      val <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
      if (is.data.frame(val)) {
        label <- rlang::as_label(quo)
        msg <- paste0(
          msg, "\n  ", fn, " does not import a data frame: `data` would be a ",
          "column called `data`. To bring ", label, " in as a level of its ",
          "own, write `import_level(", label, ", ...)`.")
      }
    }
  }
  stop(msg, call. = FALSE)
}

#' Check that `N` is a count of rows
#'
#' `N` names the number of rows a level is building, so it has to be a whole
#' positive number. Without this, `as.integer()` silently truncated: fabricate's
#' `N = 2.5` built two rows, and `declare_model(N = 2.5)` written as a way of
#' making a *column* called `N` did nothing at all and said nothing. fabricatr
#' 1.0.2 rejects both, and this restores that.
#'
#' The message is 1.0.2's, so a user who has hit it before recognizes it, with
#' the offending value added.
#'
#' @param N The evaluated value.
#' @param where The call to name in the message.
#' @param scalar Whether a single value is required, as it is for a new level.
#' @return `N` as an integer vector. On a bad value it stops rather than
#'   returning.
#' @keywords internal
#' @noRd
validate_n <- function(N, where = "fabricate()", scalar = TRUE) {
  if (!is.numeric(N) || !length(N) || anyNA(N) || any(N < 0) ||
      any(N != trunc(N))) {
    stop("Provided `N` must be positive integers.\n",
         "  ", where, " was given ", format_n_value(N), ".", call. = FALSE)
  }
  if (scalar) {
    if (length(N) != 1L) {
      stop("New level has length(N) > 1.\n",
           "  ", where, " was given ", length(N), " values; a level builds one ",
           "number of rows.", call. = FALSE)
    }
    if (N == 0) {
      stop("New level has N == 0.\n",
           "  ", where, " cannot build a level with no rows.", call. = FALSE)
    }
  }
  as.integer(N)
}

#' One-line rendering of a rejected `N`, for the message
#'
#' @keywords internal
#' @noRd
format_n_value <- function(N) {
  if (is.null(N)) return("NULL")
  if (!is.atomic(N)) return(paste0("an object of class ", class(N)[1]))
  if (length(N) > 5L) {
    return(paste0(length(N), " values starting ",
                  paste(utils::head(N, 3), collapse = ", ")))
  }
  paste(deparse(N), collapse = " ")
}

# fabricatr spellings, accepted and deprecated ---------------------------------
#
# fabricatr's `nest =` and `by =` are formals of its level constructors, so
# they arrive here inside `...` and would otherwise be captured as ordinary
# column expressions: a junk column named `nest`, and in the `by =` case an
# ungrouped answer. They used to be a hard error. They are now accepted, with a
# warning that prints the call the author should have written.
#
# The ambiguity that motivated the error is narrower than it looks. Because
# both names are formals in fabricatr, no design written against fabricatr can
# have columns called `nest` or `by`, so the only author these shims can
# confuse is one writing new fabricatr code who wants a column with one of
# those names, and the warning tells that author exactly what happened.

#' Rewrite a call for a deprecation message
#'
#' Takes the call as the author wrote it and returns the call they should have
#' written, so the warning can show both. Arguments keep their positions:
#' renaming `by` to `.by` in place reads better than dropping it and appending.
#'
#' @param cl A call, from `sys.call()`.
#' @param fn Optional new function name.
#' @param drop Names of arguments to remove.
#' @param rename Named character vector, `c(old = "new")`.
#' @param values Named list of replacement argument values.
#' @return A one-line deparsed call.
#' @keywords internal
#' @noRd
rewrite_call <- function(cl, fn = NULL, drop = character(), rename = NULL,
                         values = NULL) {
  if (!is.null(fn)) cl[[1L]] <- as.name(fn)
  for (nm in drop) cl[[nm]] <- NULL
  nms <- names(cl)
  for (old in names(rename)) {
    i <- match(old, nms)
    if (!is.na(i)) {
      if (old %in% names(values)) cl[[i]] <- values[[old]]
      names(cl)[i] <- rename[[old]]
    }
  }
  paste(trimws(deparse(cl, width.cutoff = 500L)), collapse = " ")
}

#' Warn once per offending call site
#'
#' A level constructor runs inside every simulation, so a warning per call
#' would flood a diagnosis. The frequency id is the call itself rather than a
#' fixed string, so a design with two deprecated calls reports both, each once,
#' instead of the first one silencing the second.
#'
#' Under a parallel `future` plan the first call happens in a worker and the
#' warning may not reach the console, which is why the vignette says this too
#' rather than leaving the warning to teach it.
#'
#' @keywords internal
#' @noRd
warn_deprecated_spelling <- function(what, wrote, instead) {
  rlang::warn(
    paste0(
      what, " is deprecated in fabricatr.\n",
      "Write:  ", instead, "\n",
      "Not:    ", wrote
    ),
    .frequency = "once",
    .frequency_id = paste0("fabricatr_legacy_", wrote)
  )
}

#' Resolve fabricatr's `by =` to a character vector of level names
#'
#' Accepts everything fabricatr's `by` accepted: a bare name
#' (`by = clusters`), a `join_using()` call (`by = join_using(a, b)`), and a
#' character vector.
#'
#' @keywords internal
#' @noRd
resolve_legacy_by <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  if (rlang::is_symbol(expr)) return(rlang::as_string(expr))
  if (rlang::is_call(expr, "join_using")) {
    return(vapply(as.list(expr)[-1L], function(a) {
      if (is.character(a)) a else rlang::as_string(a)
    }, character(1)))
  }
  as.character(rlang::eval_tidy(quo))
}

#' Accept fabricatr's `nest =` in a level constructor
#'
#' `add_level(..., nest = FALSE)` is fabricatr's `declare_level()`, and
#' `nest = TRUE` is `add_level()` with nothing else to say. Returns the level
#' type to build and the dots with `nest` removed.
#'
#' @keywords internal
#' @noRd
absorb_legacy_nest <- function(dots, cl, type) {
  if (!"nest" %in% names(dots)) return(list(type = type, dots = dots))
  nested <- isTRUE(rlang::eval_tidy(dots[["nest"]]))
  new_type <- if (nested) type else "declare"
  new_fn <- if (nested) NULL else "declare_level"
  warn_deprecated_spelling(
    paste0("`nest = ", if (nested) "TRUE" else "FALSE", "`"),
    wrote = paste(trimws(deparse(cl, width.cutoff = 500L)), collapse = " "),
    instead = rewrite_call(cl, fn = new_fn, drop = "nest")
  )
  list(type = new_type, dots = dots[names(dots) != "nest"])
}

#' Accept fabricatr's `by =` in a level constructor
#'
#' Returns the resolved level names and the dots with `by` removed, or `NULL`
#' for `by` when the author used `.by` as they should.
#'
#' @keywords internal
#' @noRd
absorb_legacy_by <- function(dots, cl) {
  if (!"by" %in% names(dots)) return(list(by = NULL, dots = dots))
  resolved <- resolve_legacy_by(dots[["by"]])
  warn_deprecated_spelling(
    "`by =`",
    wrote = paste(trimws(deparse(cl, width.cutoff = 500L)), collapse = " "),
    instead = rewrite_call(cl, rename = c(by = ".by"),
                           values = list(by = resolved))
  )
  list(by = resolved, dots = dots[names(dots) != "by"])
}

#' Name the levels to cross or link
#'
#' fabricatr's helper for `by = join_using(countries, years)`. In fabricatr
#' the levels are named directly, `.by = c("countries", "years")`, and this is
#' kept so designs written for fabricatr run unchanged. It returns exactly that
#' character vector, so it is also harmless in front of `.by`.
#'
#' @param ... Bare level names, or strings.
#' @return A character vector of level names.
#' @export
#' @examples
#' join_using(countries, years)
join_using <- function(...) {
  unname(vapply(rlang::ensyms(...), rlang::as_string, character(1)))
}

#' Recycle a vector to the length of the level being built
#'
#' fabricatr's helper for filling a level with a short vector. fabricatr
#' recycles automatically whenever the vector's length divides the level
#' evenly, so this is only ever a no-op made explicit, and is kept so designs
#' written for fabricatr run unchanged.
#'
#' Left off, `.N` is the `N` of the level being built. fabricatr found it by
#' walking the call stack; a fabricatr column expression is evaluated
#' against a data mask instead, so `N` is read out of the calling frame, which
#' is that mask.
#'
#' @param x A vector.
#' @param .N Length to recycle to. Left off inside a level.
#' @return `x`, recycled to length `.N`.
#' @export
#' @examples
#' fabricate(villages = add_level(N = 6, arm = recycle(c("a", "b", "c"))))
#' recycle(c("a", "b"), .N = 6)
recycle <- function(x, .N = NULL) {
  if (is.null(.N)) {
    .N <- tryCatch(get("N", envir = parent.frame(), inherits = TRUE),
                   error = function(e) NULL)
  }
  if (is.null(.N)) {
    stop("recycle() could not find `N`. Supply it as `recycle(x, .N = )`.",
         call. = FALSE)
  }
  if (length(x) == 0L || .N %% length(x) != 0L) {
    stop("recycle(): ", length(x), " values do not divide ", .N, " evenly.",
         call. = FALSE)
  }
  rep(x, length.out = .N)
}

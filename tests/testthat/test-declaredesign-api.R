# The contract DeclareDesign depends on.
#
# `fabricate_with_dots()` is called by nothing inside this package, so its only
# caller is another package and its only description of what it must do is
# here. DeclareDesign calls it at two sites in `R/declare_model.R`, with
# quosures it captured itself, precisely to avoid `fabricate()`'s own
# `enquos()` re-capturing them. Every test here is written the way those call
# sites write it, with a `dots` list built by `quos()`.
#
# It was an unexported function reached with `:::` until 2026-09-21. Nothing
# about that arrangement was safe: `R CMD check` suppresses the
# unexported-object NOTE when the two packages share a maintainer, so the
# boundary was quiet for a reason that had nothing to do with the boundary.
# It is exported now, and the first test below is what makes that a promise.

test_that("fabricate_with_dots() is exported, and DeclareDesign can call it", {
  expect_true("fabricate_with_dots" %in% getNamespaceExports("fabricatr"))
  # The argument names are half of the contract: DeclareDesign calls it by
  # name at both sites.
  expect_equal(names(formals(fabricatr::fabricate_with_dots)),
               c("data", "dots", "ID_label"))
  expect_equal(formals(fabricatr::fabricate_with_dots)$ID_label, "ID")
})

test_that("a pre-captured dots list builds what the literal call builds", {
  set.seed(343)
  direct <- fabricate(N = 20, x = rnorm(N), y = x * 2)
  set.seed(343)
  via_dots <- fabricate_with_dots(
    dots = rlang::quos(N = 20, x = rnorm(N), y = x * 2)
  )
  expect_identical(direct, via_dots)
})

test_that("splicing the same quosures into fabricate() is the path that fails", {
  # The reason the function exists. `!!!`-injection turns a quosure into a
  # formula, which fabricate()'s enquos() then captures as a column value, so
  # `N` arrives as an object of class quosure rather than as a row count. If
  # this ever stops erroring, the workaround can go.
  dots <- rlang::quos(N = 5, y = seq_len(N))
  expect_error(rlang::inject(fabricate(!!!dots)), "positive integers")
  expect_equal(nrow(fabricate_with_dots(dots = dots)), 5L)
})

test_that("N is read out of the dots and does not become a column", {
  df <- fabricate_with_dots(dots = rlang::quos(N = 4, y = seq_len(N)))
  expect_equal(nrow(df), 4L)
  expect_equal(names(df), c("ID", "y"))
  expect_false("N" %in% names(df))
})

test_that("dots apply to existing data, as a measurement step does", {
  # declare_measurement() and declare_assignment() reach fabricate_with_dots()
  # with the design's data in hand and no N.
  dat <- tibble::tibble(x = 1:4)
  df <- fabricate_with_dots(data = dat, dots = rlang::quos(y = x * 2))
  expect_equal(names(df), c("x", "y"))
  expect_equal(df$y, c(2, 4, 6, 8))
  # No ID column is stapled onto data that arrives with its own rows.
  expect_false("ID" %in% names(df))
})

test_that("an empty dots list returns the data unchanged", {
  # redesign() can leave a step with nothing left to do.
  dat <- tibble::tibble(x = 1:3)
  expect_equal(fabricate_with_dots(data = dat, dots = rlang::quos()), dat)
})

test_that("N alongside data is refused here too", {
  expect_error(
    fabricate_with_dots(data = tibble::tibble(x = 1:3),
                        dots = rlang::quos(N = 5, y = x)),
    "cannot be given alongside existing data"
  )
})

test_that("level calls survive pre-capture", {
  df <- fabricate_with_dots(dots = rlang::quos(
    villages = add_level(N = 3, u = seq_len(N)),
    citizens = nest_level(N = 2, y = u * 10)
  ))
  expect_equal(nrow(df), 6L)
  expect_equal(names(df), c("villages", "u", "citizens", "y"))
  expect_equal(df$y, rep(c(10, 20, 30), each = 2))
})

test_that("a quosure keeps the environment it was captured in", {
  # The whole point of passing quosures rather than expressions. A designer
  # builds its dots inside its own frame, and the locals of that frame have to
  # stay reachable when the step is run much later, somewhere else entirely.
  make_dots <- function(k) {
    scale <- k * 10
    rlang::quos(N = 3, y = scale * seq_len(N))
  }
  expect_equal(fabricate_with_dots(dots = make_dots(2))$y, c(20, 40, 60))
  # The N quosure is evaluated in its own environment too, not the caller's.
  make_n_dots <- function() {
    rows <- 7
    rlang::quos(N = rows)
  }
  rows <- 99
  expect_equal(nrow(fabricate_with_dots(dots = make_n_dots())), 7L)
})

test_that("ID_label is honoured, including NA", {
  # DeclareDesign's make_fabricate_step() carries an `id_label_na` flag for
  # exactly this, so the argument has to work from this side whether or not
  # that call site passes it yet.
  expect_equal(names(fabricate_with_dots(dots = rlang::quos(N = 3, y = 1:3))),
               c("ID", "y"))
  expect_equal(
    names(fabricate_with_dots(dots = rlang::quos(N = 3, y = 1:3),
                              ID_label = "unit")),
    c("unit", "y")
  )
  expect_equal(
    names(fabricate_with_dots(dots = rlang::quos(N = 3, y = 1:3),
                              ID_label = NA)),
    "y"
  )
})

test_that("an unnamed expression is refused rather than silently dropped", {
  expect_error(
    fabricate_with_dots(dots = rlang::quos(N = 3, rnorm(3))),
    "Every column needs a name"
  )
})

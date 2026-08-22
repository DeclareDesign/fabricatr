test_that("flat fabricate creates N rows", {
  df <- fabricate(N = 50, Y = rnorm(N), X = rbinom(N, 1, 0.5))
  expect_equal(nrow(df), 50L)
  expect_true(all(c("Y", "X") %in% names(df)))
})

test_that("N is not a persistent column", {
  df <- fabricate(N = 10, Y = rnorm(N))
  expect_false("N" %in% names(df))
})

test_that("sequential column access works", {
  df <- fabricate(N = 20, X = rnorm(N), Y = X + rnorm(N))
  expect_equal(nrow(df), 20L)
  expect_true(all(c("X", "Y") %in% names(df)))
})

test_that("fabricate from existing data works", {
  base <- data.frame(x = 1:10)
  df <- fabricate(data = base, y = x^2)
  expect_equal(df$y, (1:10)^2)
})

test_that("multi-column output via data frame return is bind_col'd", {
  df <- fabricate(N = 5,
                  potential_outcomes(A ~ Z + 1, conditions = list(Z = 0:1)))
  expect_true("A_Z_0" %in% names(df))
  expect_true("A_Z_1" %in% names(df))
})

test_that("flat fabricate creates a zero-padded character ID column", {
  df <- fabricate(N = 10, Y = rnorm(N))
  expect_equal(names(df), c("ID", "Y"))
  expect_equal(df$ID, c(paste0("0", 1:9), "10"))
})

test_that("ID_label renames and NA suppresses the ID column", {
  expect_equal(names(fabricate(N = 5, Y = rnorm(N), ID_label = "unit")),
               c("unit", "Y"))
  expect_equal(names(fabricate(N = 5, Y = rnorm(N), ID_label = NA)), "Y")
})

test_that("ID is visible to column expressions and can be overwritten", {
  expect_equal(fabricate(N = 5, x = paste0("u", ID))$x, paste0("u", 1:5))
  df <- fabricate(N = 3, ID = letters[1:3])
  expect_equal(df$ID, letters[1:3])
  expect_equal(ncol(df), 1L)
})

test_that("no flat ID column when levels or data are used", {
  expect_false("ID" %in% names(fabricate(g = add_level(N = 3, x = rnorm(N)))))
  expect_false("ID" %in% names(fabricate(data = data.frame(x = 1:5), y = x^2)))
})

test_that("unnamed level and multi-column calls are not swallowed as N", {
  df <- fabricate(data = data.frame(U = rnorm(5)),
                  potential_outcomes(Y ~ 0.5 * Z + U))
  expect_true(all(c("Y_Z_0", "Y_Z_1") %in% names(df)))

  d2 <- fabricate(N = 12, g = rep(1:3, 4), Y = rnorm(N),
                  modify_level(gm = mean(Y), .by = "g"))
  expect_true("gm" %in% names(d2))
  expect_equal(d2$gm[d2$g == 1][1], mean(d2$Y[d2$g == 1]))
})

test_that("fabricatr legacy arguments are accepted and deprecated", {
  # These used to be hard errors. They now work, and warn with the rewrite:
  # see tests/testthat/test-deprecated.R.
  rlang::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(
    d1 <- fabricate(a = add_level(N = 2, x = rnorm(N), nest = FALSE)),
    "`nest = FALSE` is deprecated"
  )
  expect_equal(nrow(d1), 2L)
  expect_false("nest" %in% names(d1))

  expect_warning(
    d2 <- fabricate(N = 4, g = rep(1:2, 2), Y = rnorm(N),
                    modify_level(m = mean(Y), by = "g")),
    "`by =` is deprecated"
  )
  expect_equal(d2$m[d2$g == 1][1], mean(d2$Y[d2$g == 1]))
})

test_that("padded ID width follows the number of units at that level", {
  df <- fabricate(clusters = add_level(N = 12), units = add_level(N = 2))
  expect_equal(df$clusters[1], "01")
  expect_equal(df$units[24], "24")
  expect_equal(fabricate(N = 5, Y = rnorm(N))$ID, as.character(1:5))
})

test_that("a matrix column is split so later expressions can use its columns", {
  # fabricatr#188
  set.seed(1)
  df <- fabricate(N = 4, X = matrix(rnorm(12), 4), Y = X.1)
  expect_equal(names(df), c("ID", "X.1", "X.2", "X.3", "Y"))
  expect_equal(df$Y, df$X.1)

  named <- fabricate(N = 3,
                     X = matrix(rnorm(6), 3, dimnames = list(NULL, c("a", "b"))))
  expect_equal(names(named), c("ID", "X.a", "X.b"))

  # a single-column matrix stays one plain column
  expect_equal(names(fabricate(N = 3, X = matrix(rnorm(3), 3))), c("ID", "X"))

  # and it works inside a level, where each split column must fill the level
  lev <- fabricate(g = add_level(N = 2),
                   u = nest_level(N = 2, X = matrix(rnorm(8), 4), Z = X.2))
  expect_equal(names(lev), c("g", "u", "X.1", "X.2", "Z"))
  expect_equal(lev$Z, lev$X.2)
})

test_that("n() counts the rows the level is building", {
  expect_equal(nrow(fabricate(N = 7, Y = rnorm(n()))), 7L)
  expect_equal(unique(fabricate(N = 7, k = n())$k), 7L)

  # each level counts its own rows
  df <- fabricate(
    villages = add_level(N = 4, v = n()),
    citizens = nest_level(N = 3, c = n())
  )
  expect_equal(unique(df$v), 4L)
  expect_equal(unique(df$c), 12L)
  expect_equal(unique(df$c), nrow(df))

  # and a modified level counts the group it is in
  grouped <- fabricate(N = 6, g = rep(1:2, each = 3),
                       k = modify_level(.by = "g", k = n()))
  expect_equal(unique(grouped$k), 3L)
})

test_that("a column called n does not stop n() from working", {
  df <- fabricate(N = 5, n = 10:14, total = sum(n), size = n())
  expect_equal(df$n, 10:14)
  expect_equal(unique(df$total), 60L)
  expect_equal(unique(df$size), 5L)
})

test_that("n() beats a same-named function on the search path", {
  skip_if_not_installed("dplyr")
  # dplyr::n() errors outside a dplyr verb, so a declaration writing n() would
  # fail if the search path won the lookup.
  local({
    library(dplyr)
    expect_equal(unique(fabricate(N = 3, k = n())$k), 3L)
  })
})

test_that("a variable already called n keeps its meaning", {
  # From ResearchDesigns, where several designs use `n` as a sample size.
  # Binding n() below the data but above the design's own scope would shadow
  # them, and `n < 0` on a function is not a useful error.
  local({
    n <- 5
    expect_equal(nrow(fabricate(N = n, Y = rnorm(n))), 5L)
    expect_equal(unique(fabricate(N = 4, k = n)$k), 5L)
  })
})

test_that("N alongside existing data is refused, as in 1.0.2", {
  # `declare_model(N = m, U = rnorm(N)) + declare_model(N = 2.5)` ran clean and
  # did nothing: the row count is fixed by the data, so the second `N` could
  # only have meant a column called `N`, and it was dropped in silence.
  base <- fabricate(N = 3, x = 1:3)
  expect_error(fabricate(data = base, N = 2.5), "cannot be given alongside")
  expect_error(fabricate(data = base, N = 5), "cannot be given alongside")
  # data plus new variables, and N alone, both still work
  expect_equal(nrow(fabricate(data = base, y = x * 2)), 3L)
  expect_equal(nrow(fabricate(N = 5)), 5L)
})

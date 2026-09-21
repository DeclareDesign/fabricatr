test_that("declare_level and cross_levels produce Cartesian product", {
  df <- fabricate(
    countries = declare_level(N = 4, gdp = runif(N, 1, 10)),
    years     = declare_level(N = 3, shock = runif(N, 0, 1)),
    obs       = cross_levels(.by = c("countries", "years"),
                             Y = gdp + shock)
  )
  expect_equal(nrow(df), 12L)
  expect_true("Y" %in% names(df))
})

test_that("cross_levels errors on missing level name", {
  expect_error(
    fabricate(
      A = declare_level(N = 3),
      obs = cross_levels(.by = c("A", "B"))
    ),
    "not found in registry"
  )
})

test_that("a level modified at its own level carries the column into a cross", {
  df <- fabricate(
    countries = declare_level(N = 2, g = 1:2),
    countries = modify_level(g10 = g * 10),
    years     = declare_level(N = 2, t = 1:2),
    obs       = cross_levels(.by = c("countries", "years"), Y = g10 + t)
  )
  expect_equal(df$Y, c(11, 21, 12, 22))
})

test_that("link_levels samples N rows from cross product", {
  df <- fabricate(
    primary   = declare_level(N = 10, pq = runif(N)),
    secondary = declare_level(N = 8,  sq = runif(N)),
    students  = link_levels(N = 50, .by = c("primary", "secondary"),
                            score = pq + sq)
  )
  expect_equal(nrow(df), 50L)
  expect_true("score" %in% names(df))
})

test_that("link_levels with rho produces correlated assignments", {
  set.seed(343)
  df <- fabricate(
    A = declare_level(N = 100, a_val = seq(0, 1, length.out = N)),
    B = declare_level(N = 100, b_val = seq(0, 1, length.out = N)),
    obs = link_levels(N = 500, .by = c("A", "B"), rho = 0.8,
                      x = a_val + b_val)
  )
  expect_equal(nrow(df), 500L)
  # Positive rho: units with high a_val should tend to pair with high b_val
  expect_gt(cor(df$a_val, df$b_val, method = "spearman"), 0.4)
})

test_that("link_levels accepts a correlation matrix beyond two levels", {
  # Values verified identical to fabricatr 1.0.2 from the same seed.
  set.seed(31)
  df <- fabricate(
    a = declare_level(N = 8, xa = runif(N)),
    b = declare_level(N = 9, xb = runif(N)),
    c = declare_level(N = 7, xc = runif(N)),
    obs = link_levels(N = 3000, .by = c("a", "b", "c"),
                      sigma = matrix(c(1, .5, .3, .5, 1, .4, .3, .4, 1), 3, 3))
  )
  expect_equal(nrow(df), 3000L)
  ids <- sapply(df[c("a", "b", "c")], as.numeric)
  observed <- cor(ids, method = "spearman")
  expect_equal(round(observed[1, 2], 4), 0.4602)
  expect_equal(round(observed[1, 3], 4), 0.2697)
  expect_equal(round(observed[2, 3], 4), 0.3703)
})

test_that("link_levels rejects correlation matrices it cannot draw from", {
  three <- function(sigma, ...) {
    fabricate(
      a = declare_level(N = 5, xa = runif(N)),
      b = declare_level(N = 5, xb = runif(N)),
      c = declare_level(N = 5, xc = runif(N)),
      obs = link_levels(N = 50, .by = c("a", "b", "c"), sigma = sigma, ...)
    )
  }
  psd_fail <- matrix(c(1, -.9, -.9, -.9, 1, -.9, -.9, -.9, 1), 3, 3)
  expect_error(three(psd_fail), "positive semi-definite")
  expect_error(three(matrix(c(1, 1.5, .3, 1.5, 1, .4, .3, .4, 1), 3, 3)),
               "between -1 and 1")
  expect_error(three(matrix(c(1, .5, .5, 1), 2, 2)), "one row and one column")
  expect_error(three(matrix(c(1, .5, .3, .2, 1, .4, .3, .4, 1), 3, 3)),
               "symmetric")
})

test_that("a single negative rho is refused for three or more levels", {
  expect_error(
    fabricate(
      a = declare_level(N = 5, xa = runif(N)),
      b = declare_level(N = 5, xb = runif(N)),
      c = declare_level(N = 5, xc = runif(N)),
      obs = link_levels(N = 50, .by = c("a", "b", "c"), rho = -0.5)
    ),
    "positive semi-definite"
  )
})

test_that("the correlated draw does not depend on optional packages", {
  # Regression test. fabricatr, and fabricatr until now, switched to
  # mvnfast::rmvn() whenever that package happened to be installed. The two
  # paths consume the RNG differently, so the same seed gave different data on
  # different machines, and three tests above silently changed answer the day
  # mvnfast was installed. There is now one path.
  draw <- function() {
    set.seed(31)
    fabricate(
      a = declare_level(N = 8, xa = runif(N)),
      b = declare_level(N = 9, xb = runif(N)),
      obs = link_levels(N = 500, .by = c("a", "b"), rho = 0.5)
    )
  }
  expect_equal(draw(), draw())
  expect_false("mvnfast" %in% names(packageDescription("fabricatr")))
  expect_equal(
    length(grep("mvnfast", readLines(system.file("DESCRIPTION",
                                                 package = "fabricatr")))),
    0L
  )
})

test_that("link_levels keeps rho and sigma behind the dots", {
  # With `rho` ahead of `...`, a column called `r`, `s` or `si` was partially
  # matched into `rho` or `sigma` and vanished.
  df <- fabricate(
    a   = declare_level(N = 5, x = rnorm(N)),
    b   = declare_level(N = 4, y = rnorm(N)),
    obs = link_levels(N = 20, .by = c("a", "b"), s = x + y, r = 1, si = 2)
  )
  expect_equal(names(df), c("a", "x", "b", "y", "obs", "s", "r", "si"))
  expect_equal(df$s, df$x + df$y)
  expect_equal(names(formals(link_levels)), c("N", ".by", "...", "rho", "sigma"))
})

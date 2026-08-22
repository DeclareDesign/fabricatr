test_that("add_level creates correct row count and ID column", {
  df <- fabricate(villages = add_level(N = 10, income = rnorm(N)))
  expect_equal(nrow(df), 10L)
  expect_true("villages" %in% names(df))
  expect_true("income" %in% names(df))
  expect_false("N" %in% names(df))
})

test_that("nest_level fans out rows correctly (scalar N)", {
  df <- fabricate(
    villages = add_level(N = 5, v_inc = rnorm(N)),
    citizens = nest_level(N = 10, c_inc = v_inc + rnorm(N))
  )
  expect_equal(nrow(df), 50L)
  expect_true(all(c("villages", "citizens", "v_inc", "c_inc") %in% names(df)))
})

test_that("N inside a nested level is the level's total row count", {
  df <- fabricate(
    blocks = add_level(N = 3),
    units  = nest_level(N = 4, Z = rep(0:1, N / 2))
  )
  # N = 12, so rep(0:1, 6) fills the level and each block still gets 0,1,0,1
  expect_equal(nrow(df), 12L)
  expect_setequal(df$Z, c(0L, 1L))
  expect_equal(as.vector(tapply(df$Z, df$blocks, sum)), rep(2L, 3))
})

test_that("nested draws are independent across parent groups", {
  # Regression test. Evaluating the expression once and repeating it across
  # parents gave every village the identical residuals, so any clustered
  # design built this way had perfectly correlated within-cluster noise.
  set.seed(1)
  df <- fabricate(
    villages = add_level(N = 40, u = rnorm(N)),
    citizens = nest_level(N = 8, e = rnorm(N))
  )
  by_village <- split(df$e, df$villages)
  expect_equal(length(unique(lapply(by_village, identity))), 40L)
  m <- do.call(cbind, by_village)
  expect_lt(abs(mean(cor(m)[lower.tri(cor(m))])), 0.2)
})

test_that("a short vector written out deliberately still recycles", {
  # The book numbers tasks within each subject this way, and fabricatr
  # recycles it. Safe because N is the level total, so rnorm(N) and friends
  # already return one value per row and never reach the recycling path.
  df <- fabricate(
    subject = add_level(N = 4),
    task    = nest_level(N = 3, task = 1:3)
  )
  expect_equal(df$task, rep(1:3, 4))
})

test_that("a nested column that cannot fill the level is an error", {
  expect_error(
    fabricate(
      villages = add_level(N = 3, u = rnorm(N)),
      citizens = nest_level(N = 4, e = rnorm(5))
    ),
    "does not fill the level"
  )
})

test_that("nest_level supports variable per-parent N", {
  df <- fabricate(
    countries = add_level(N = 3, n_cities = c(2L, 3L, 4L)),
    cities    = nest_level(N = n_cities, gdp = rnorm(N))
  )
  expect_equal(nrow(df), 9L)  # 2+3+4
  expect_equal(length(unique(df$gdp)), 9L)
})

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
  set.seed(42)
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

test_that("modify_level adds columns to existing level", {
  df <- fabricate(
    N = 20,
    cluster = sample(1:4, N, replace = TRUE),
    Y = rnorm(N),
    updated = modify_level(Y2 = Y * 2)
  )
  expect_true("Y2" %in% names(df))
  expect_equal(df$Y2, df$Y * 2)
})

test_that("modify_level with .by does grouped operation", {
  df <- fabricate(
    N = 20,
    g = rep(1:4, each = 5),
    Y = rnorm(N),
    upd = modify_level(gm = mean(Y), .by = "g")
  )
  # All rows in the same group should have the same gm
  expect_equal(
    df$gm[df$g == 1],
    rep(mean(df$Y[df$g == 1]), 5)
  )
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

test_that("N must be a whole positive number of rows", {
  # fabricatr 1.0.2 rejects all of these; `as.integer()` used to truncate them
  # silently, so `fabricate(N = 2.5)` built two rows and said nothing.
  expect_error(fabricate(N = 2.5), "must be positive integers")
  expect_error(fabricate(N = pi), "must be positive integers")
  expect_error(fabricate(N = -3), "must be positive integers")
  expect_error(fabricate(N = NA), "must be positive integers")
  expect_error(fabricate(N = "10"), "must be positive integers")
  expect_error(fabricate(N = 0), "N == 0")
  expect_error(fabricate(N = c(2, 3)), "length\\(N\\) > 1")
  # the message names the call and the value, so the fix is visible
  expect_error(fabricate(a = add_level(N = 2.5)), "add_level\\(\\) was given 2.5")
})

test_that("a valid N is unchanged by the check", {
  expect_equal(nrow(fabricate(N = 10)), 10L)
  expect_equal(nrow(fabricate(N = 10L)), 10L)
  expect_equal(nrow(fabricate(a = add_level(N = 3))), 3L)
  # a per-parent vector is still allowed at a nested level
  expect_equal(nrow(fabricate(a = add_level(N = 2), b = nest_level(N = c(1, 3)))), 4L)
  expect_error(fabricate(a = add_level(N = 2), b = nest_level(N = 2.5)),
               "nest_level\\(\\) was given 2.5")
})

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

test_that("a level's expressions see the author's environment, not the frame", {
  # Level calls are evaluated inside the mask of the frame in hand, so their
  # quosures are re-homed to where the author wrote them. A local of a
  # function that builds the level stays reachable, and so does a variable
  # next to the fabricate() call.
  build <- function(k) add_level(N = 3, x = k * seq_len(N))
  expect_equal(fabricate(a = build(10))$x, c(10, 20, 30))
  mult <- 5
  df <- fabricate(
    regions = add_level(N = 2),
    cities  = nest_level(N = 2, y = mult * seq_len(N)),
    regions = modify_level(z = mult * seq_len(N))
  )
  expect_equal(df$y, c(5, 10, 15, 20))
  expect_equal(df$z, c(5, 5, 10, 10))
})

test_that("nest_level needs a parent and an N that matches it", {
  expect_error(fabricate(u = nest_level(N = 2, x = 1)),
               "requires an existing level to nest within")
  expect_error(
    fabricate(g = add_level(N = 3), u = nest_level(N = c(1, 2), x = 1)),
    "scalar or a vector of length nrow\\(parent\\)")
  d <- fabricate(g = add_level(N = 3), u = nest_level(N = c(1, 2, 3), x = 1))
  expect_equal(nrow(d), 6L)
})

test_that("an unnamed data frame inside a level splats into columns", {
  # Tested flat, where the whole frame is the level; inside a level the columns
  # go through recycle_to_level() instead, which is a different code path.
  skip_if_not_installed("MASS")
  S <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
  set.seed(343)
  d <- fabricate(
    g = add_level(N = 3),
    u = nest_level(N = 2, draw_multivariate(c(A, B) ~ MASS::mvrnorm(N, c(0, 0), S))))
  expect_equal(names(d), c("g", "u", "A", "B"))
  expect_equal(nrow(d), 6L)
  # One value per unit of the nested level, not one per parent recycled.
  expect_equal(length(unique(d$A)), 6L)
  # A one-row frame still recycles to the level.
  d2 <- fabricate(g = add_level(N = 2), u = nest_level(N = 3, data.frame(k = 1)))
  expect_equal(d2$k, rep(1, 6))
})

test_that("an unnamed data frame splats inside add_level too", {
  # add_level() reaches the splat through eval_dots_into_list(), where the
  # columns are recycled with recycle_to_n() rather than recycle_to_level().
  skip_if_not_installed("MASS")
  S <- matrix(c(1, 0.4, 0.4, 1), 2, 2)
  set.seed(343)
  d <- fabricate(g = add_level(
    N = 4, draw_multivariate(c(A, B) ~ MASS::mvrnorm(N, c(0, 0), S))))
  expect_equal(names(d), c("g", "A", "B"))
  expect_equal(nrow(d), 4L)
  expect_equal(length(unique(d$A)), 4L)
  # A single row recycles to the level's N.
  expect_equal(fabricate(g = add_level(N = 3, data.frame(k = 7)))$k, rep(7, 3))
})

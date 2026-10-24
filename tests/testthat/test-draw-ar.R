lag_cor <- function(e, clusters, time, lag = 1) {
  ord <- order(clusters, time)
  e <- e[ord]
  clusters <- clusters[ord]
  time <- time[ord]
  n <- length(e)
  keep <- clusters[-(1:lag)] == clusters[-((n - lag + 1):n)] &
    time[-(1:lag)] - time[-((n - lag + 1):n)] == lag
  cor(e[-(1:lag)][keep], e[-((n - lag + 1):n)][keep])
}

test_that("draw_normal_ar produces the target serial correlation", {
  set.seed(343)
  cl <- rep(1:500, each = 10)
  tm <- rep(1:10, times = 500)
  e <- draw_normal_ar(clusters = cl, time = tm, rho = 0.7)
  expect_equal(lag_cor(e, cl, tm, 1), 0.7, tolerance = 0.05)
  expect_equal(lag_cor(e, cl, tm, 2), 0.49, tolerance = 0.05)
})

test_that("the process is stationary from the first period", {
  set.seed(343)
  cl <- rep(1:2000, each = 5)
  tm <- rep(1:5, times = 2000)
  e <- draw_normal_ar(clusters = cl, time = tm, rho = 0.9, mean = 2, sd = 3)
  by_period_sd <- tapply(e, tm, sd)
  expect_true(all(abs(by_period_sd - 3) < 0.2))
  expect_equal(mean(e), 2, tolerance = 0.1)
})

test_that("a gap in time decays the correlation as rho to the gap", {
  set.seed(343)
  cl <- rep(1:5000, each = 2)
  tm <- rep(c(1, 3), times = 5000)
  e <- draw_normal_ar(clusters = cl, time = tm, rho = 0.8)
  expect_equal(cor(e[tm == 1], e[tm == 3]), 0.64, tolerance = 0.05)
})

test_that("negative rho alternates in sign", {
  set.seed(343)
  cl <- rep(1:500, each = 10)
  tm <- rep(1:10, times = 500)
  e <- draw_normal_ar(clusters = cl, time = tm, rho = -0.6)
  expect_equal(lag_cor(e, cl, tm, 1), -0.6, tolerance = 0.05)
})

test_that("results come back in the order the rows were given", {
  cl <- rep(c("a", "b", "c"), each = 4)
  tm <- rep(1:4, times = 3)
  set.seed(343)
  sorted <- draw_normal_ar(clusters = cl, time = tm, rho = 0.5)
  shuffle <- sample(12)
  set.seed(343)
  shuffled <- draw_normal_ar(clusters = cl[shuffle], time = tm[shuffle],
                             rho = 0.5)
  expect_equal(shuffled, sorted[shuffle])
})

test_that("rho of 0 gives independent draws", {
  set.seed(343)
  e <- draw_normal_ar(clusters = rep(1, 5), time = 1:5, rho = 0)
  set.seed(343)
  expect_equal(e, rnorm(5))
})

test_that("draw_normal_ar reads the character IDs cross_levels produces", {
  set.seed(343)
  panel <- fabricate(
    units = declare_level(N = 3),
    periods = declare_level(N = 12),
    obs = cross_levels(
      .by = c("units", "periods"),
      e = draw_normal_ar(clusters = units, time = periods, rho = 0.5)
    )
  )
  expect_type(panel$periods, "character")
  expect_length(panel$e, 36L)
  expect_true(all(is.finite(panel$e)))
})

test_that("a Date time is read in days", {
  set.seed(343)
  cl <- rep(1:5000, each = 2)
  tm <- rep(as.Date(c("2026-01-01", "2026-01-03")), times = 5000)
  e <- draw_normal_ar(clusters = cl, time = tm, rho = 0.8)
  expect_equal(cor(e[tm == tm[1]], e[tm == tm[2]]), 0.64, tolerance = 0.05)

  set.seed(343)
  by_date <- draw_normal_ar(clusters = cl, time = tm, rho = 0.8)
  set.seed(343)
  by_day <- draw_normal_ar(clusters = cl, time = as.numeric(tm), rho = 0.8)
  expect_equal(by_date, by_day)
})

test_that("a factor time is read by its labels, not its level order", {
  cl <- rep(1, 3)
  tm <- factor(c("10", "2", "1"))
  set.seed(343)
  by_factor <- draw_normal_ar(clusters = cl, time = tm, rho = 0.5)
  set.seed(343)
  by_number <- draw_normal_ar(clusters = cl, time = c(10, 2, 1), rho = 0.5)
  expect_equal(by_factor, by_number)
})

test_that("draw_normal_ar refuses what it cannot draw", {
  cl <- rep(1:2, each = 3)
  tm <- rep(1:3, times = 2)
  expect_error(draw_normal_ar(cl, tm, rho = 1), "strictly between")
  expect_error(draw_normal_ar(cl, tm, rho = -1), "strictly between")
  expect_error(draw_normal_ar(cl, tm, rho = c(0.1, 0.2)), "strictly between")
  expect_error(draw_normal_ar(cl, tm, rho = 0.5, sd = -1), "non-negative")
  expect_error(draw_normal_ar(cl, tm, rho = 0.5, mean = 1:4), "`mean`")
  expect_error(draw_normal_ar(cl, tm[-1], rho = 0.5), "one value per row")
  expect_error(draw_normal_ar(cl, c(1, 1, 2, 1, 2, 3), rho = 0.5),
               "same `time`")
  expect_error(draw_normal_ar(cl, c("a", "b", "c", 1, 2, 3), rho = 0.5),
               "read as numbers")
  expect_error(draw_normal_ar(cl, c(1, NA, 3, 1, 2, 3), rho = 0.5),
               "missing")
  expect_error(draw_normal_ar(c(1, NA, 1, 2, 2, 2), tm, rho = 0.5),
               "missing")
  expect_error(draw_normal_ar(cl, c(1, 1.5, 3, 1, 2, 3), rho = -0.5),
               "whole-number")
  expect_length(draw_normal_ar(cl, c(1, 1.5, 3, 1, 2, 3), rho = 0.5), 6L)
  expect_length(draw_normal_ar(integer(0), numeric(0), rho = 0.5), 0L)
})

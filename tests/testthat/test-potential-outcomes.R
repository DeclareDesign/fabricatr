test_that("potential_outcomes creates two columns for binary treatment", {
  df <- fabricate(
    N = 20, U = rnorm(N),
    potential_outcomes(Y ~ 0.5 * Z + U)
  )
  expect_true("Y_Z_0" %in% names(df))
  expect_true("Y_Z_1" %in% names(df))
  expect_equal(df$Y_Z_0, df$U)
  expect_equal(df$Y_Z_1, 0.5 + df$U)
})

test_that("potential_outcomes works with three conditions", {
  df <- fabricate(
    N = 10,
    potential_outcomes(Y ~ Z * 2, conditions = list(Z = 0:2))
  )
  expect_true(all(c("Y_Z_0", "Y_Z_1", "Y_Z_2") %in% names(df)))
  expect_equal(df$Y_Z_2, rep(4, 10))
})

test_that("potential_outcomes supports multi-arm factorial", {
  df <- fabricate(
    N = 8,
    potential_outcomes(Y ~ Z1 + Z2, conditions = list(Z1 = 0:1, Z2 = 0:1))
  )
  expect_equal(length(grep("^Y_", names(df))), 4L)
})

test_that("reveal_outcomes selects correct column per unit", {
  dat <- fabricate(
    N = 100, U = rnorm(N),
    potential_outcomes(Y ~ 0.5 * Z + U)
  )
  dat <- fabricate(
    data = dat,
    Z = rbinom(N, 1, 0.5),
    Y = reveal_outcomes(Y ~ Z)
  )
  expect_equal(dat$Y[dat$Z == 0], dat$Y_Z_0[dat$Z == 0])
  expect_equal(dat$Y[dat$Z == 1], dat$Y_Z_1[dat$Z == 1])
})

test_that("reveal_outcomes works with factorial treatments", {
  dat <- fabricate(
    N = 50,
    potential_outcomes(Y ~ Z1 * 0.3 + Z2 * 0.5,
                       conditions = list(Z1 = 0:1, Z2 = 0:1))
  )
  dat <- fabricate(
    data = dat,
    Z1 = rbinom(N, 1, 0.5),
    Z2 = rbinom(N, 1, 0.5),
    Y  = reveal_outcomes(Y ~ Z1 + Z2)
  )
  expect_length(dat$Y, 50L)
})

test_that("resample_data simple bootstrap returns correct nrow", {
  df <- fabricate(N = 40, Y = rnorm(N))
  boot <- resample_data(df)
  expect_equal(nrow(boot), 40L)
})

test_that("resample_data cluster bootstrap resamples correct clusters", {
  df <- fabricate(
    clusters = add_level(N = 10),
    units    = nest_level(N = 5, Y = rnorm(N))
  )
  boot <- resample_data(df, N = c(clusters = 6))
  # 6 draws (with replacement) * 5 units = 30 rows
  expect_equal(nrow(boot), 30L)
  # Unique clusters may be < 6 due to replacement — just bound from above
  expect_lte(length(unique(boot$clusters)), 6L)
})

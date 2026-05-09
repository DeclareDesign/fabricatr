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

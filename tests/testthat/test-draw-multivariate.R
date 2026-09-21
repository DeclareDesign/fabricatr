test_that("draw_multivariate returns tibble with named columns", {
  skip_if_not_installed("MASS")
  S <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  df <- draw_multivariate(c(X, Y) ~ MASS::mvrnorm(n = 100, mu = c(0, 0), Sigma = S))
  expect_s3_class(df, "tbl_df")
  expect_equal(ncol(df), 2L)
  expect_equal(nrow(df), 100L)
  expect_setequal(names(df), c("X", "Y"))
})

test_that("draw_multivariate prefix naming works", {
  skip_if_not_installed("MASS")
  S <- diag(3)
  df <- draw_multivariate(V ~ MASS::mvrnorm(n = 50, mu = rep(0, 3), Sigma = S))
  expect_setequal(names(df), c("V_1", "V_2", "V_3"))
})

test_that("correlate produces positively correlated output", {
  set.seed(1)
  x <- rnorm(500)
  y <- correlate(draw_binary, prob = 0.5, given = x, rho = 0.6)
  expect_gt(cor(x, y, method = "spearman"), 0.3)
})

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

test_that("draw_multivariate validates both sides of the formula", {
  skip_if_not_installed("MASS")
  S <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  expect_error(draw_multivariate(c(A, B) ~ rnorm(10)),
               "must return a matrix or data frame")
  expect_error(draw_multivariate(~ MASS::mvrnorm(5, c(0, 0), S)),
               "Provide column names on the LHS")
  expect_error(draw_multivariate(c(A, B, C) ~ MASS::mvrnorm(5, c(0, 0), S)),
               "LHS names \\(3\\) do not match matrix columns \\(2\\)")
  expect_error(draw_multivariate(1 ~ MASS::mvrnorm(5, c(0, 0), S)),
               "Unrecognised LHS")
})

test_that("draw_multivariate accepts a data frame RHS and a custom separator", {
  skip_if_not_installed("MASS")
  S <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
  expect_equal(names(draw_multivariate(
    Y ~ MASS::mvrnorm(5, c(0, 0), S), sep = ".")), c("Y.1", "Y.2"))
  expect_equal(names(draw_multivariate(
    c(A, B) ~ as.data.frame(MASS::mvrnorm(5, c(0, 0), S)))), c("A", "B"))
})

test_that("correlate validates its handler, rho and given", {
  g <- rnorm(20)
  expect_error(correlate("rnorm", given = g, rho = 0.5), "must be a function")
  expect_error(correlate(rnorm, given = g, rho = 2), "single number in \\[-1, 1\\]")
  expect_error(correlate(rnorm, given = g, rho = c(0.5, 0.5)),
               "single number in \\[-1, 1\\]")
  # `rho < -1` is NA for an NA rho, so this used to reach `if (NA)`.
  expect_error(correlate(rnorm, given = g, rho = NA_real_),
               "single number in \\[-1, 1\\]")
  expect_error(correlate(rnorm, given = NULL, rho = 0.5), "non-null vector")
  expect_error(correlate(rnorm, given = matrix(1:10, 5), rho = 0.5),
               "non-null vector")
  # A closure with neither a quantile_y formal nor a q* counterpart.
  expect_error(correlate(stats::rlogis, given = g, rho = 0.5),
               "must be a draw_\\*\\(\\) function or a base R r\\*\\(\\) function")
})

test_that("correlate routes base R r* generators through their q* counterpart", {
  set.seed(343)
  g <- rnorm(400)
  y <- correlate(rnorm, mean = 10, sd = 2, given = g, rho = 0.8)
  expect_length(y, 400L)
  expect_gt(cor(g, y, method = "spearman"), 0.6)
  expect_equal(mean(y), 10, tolerance = 0.3)
  counts <- correlate(rpois, lambda = 4, given = g, rho = 0.8)
  expect_true(all(counts == round(counts)))
  expect_gt(cor(g, counts, method = "spearman"), 0.6)
  # Arguments beyond the first are forwarded, so a two-parameter family works.
  expect_true(all(correlate(rbinom, size = 10, prob = 0.5,
                            given = g, rho = 0.7) %in% 0:10))
})

test_that("every r* in the lookup table maps to its own q*", {
  # A misalignment here would be silent: the draw would come from the wrong
  # family at plausible-looking values rather than error.
  fams <- c("beta", "binom", "cauchy", "chisq", "exp", "f", "gamma", "geom",
            "hyper", "lnorm", "nbinom", "norm", "pois", "t", "unif", "weibull")
  for (fam in fams) {
    q <- fabricatr:::lookup_quantile_function(get(paste0("r", fam), envir = asNamespace("stats")))
    expect_identical(q, get(paste0("q", fam), envir = asNamespace("stats")),
                     info = fam)
  }
  expect_null(fabricatr:::lookup_quantile_function(stats::rlogis))
})

test_that("an NA in given yields an NA in the result rather than a top rank", {
  # rank() defaults to na.last = TRUE, so an NA took the highest rank and drew
  # a correspondingly extreme value: the missingness vanished into a number
  # that looked real, and the returned vector had no NA anywhere.
  set.seed(343)
  g <- rnorm(50)
  g[c(3, 11)] <- NA
  y <- correlate(rnorm, mean = 0, given = g, rho = 0.9)
  expect_length(y, 50L)
  expect_equal(which(is.na(y)), c(3L, 11L))
  expect_gt(cor(g, y, method = "spearman", use = "complete.obs"), 0.6)
})

test_that("the no-NA path draws the same numbers as ranking the whole vector", {
  set.seed(343)
  g <- rnorm(30)
  rho <- 0.6
  set.seed(1)
  expected <- rnorm(30, rho * qnorm(rank(g) / 31), sqrt(1 - rho^2))
  set.seed(1)
  # correlate(rnorm, ...) returns qnorm(pnorm(sn_y)), which is sn_y.
  expect_equal(correlate(rnorm, given = g, rho = rho), expected)
})

test_that("draw_binary returns 0/1 vector of right length", {
  x <- draw_binary(prob = 0.5, N = 100)
  expect_length(x, 100L)
  expect_true(all(x %in% c(0L, 1L)))
})

test_that("draw_binary with logit link works", {
  x <- draw_binary(latent = rnorm(50), link = "logit")
  expect_length(x, 50L)
  expect_true(all(x %in% c(0L, 1L)))
})

test_that("draw_binary with probit link works", {
  x <- draw_binary(latent = rnorm(50), link = "probit")
  expect_length(x, 50L)
})

test_that("draw_binomial returns values in [0, trials]", {
  x <- draw_binomial(prob = 0.3, trials = 10, N = 200)
  expect_length(x, 200L)
  expect_true(all(x >= 0 & x <= 10))
})

test_that("draw_count returns non-negative integers", {
  x <- draw_count(mean = 5, N = 100)
  expect_length(x, 100L)
  expect_true(all(x >= 0))
})

test_that("draw_categorical returns integers in correct range", {
  x <- draw_categorical(prob = c(0.2, 0.5, 0.3), N = 200)
  expect_length(x, 200L)
  expect_true(all(x %in% 1:3))
})

test_that("draw_categorical with labels returns ordered factor", {
  x <- draw_categorical(prob = c(0.3, 0.4, 0.3), N = 50,
                        labels = c("low", "mid", "high"))
  expect_s3_class(x, "factor")
  expect_true(is.ordered(x))
})

test_that("draw_categorical normalises rows", {
  # unnormalised probs sum to 2; should still work
  x <- draw_categorical(prob = c(0.4, 1.0, 0.6), N = 100)
  expect_true(all(x %in% 1:3))
})

test_that("draw_ordered cuts latent at breaks", {
  x <- draw_ordered(rnorm(100), breaks = c(-1, 0, 1))
  expect_true(all(x %in% 1:4))
})

test_that("draw_ordered with labels returns ordered factor", {
  x <- draw_ordered(rnorm(50), breaks = c(-1, 1),
                    labels = c("low", "mid", "high"))
  expect_s3_class(x, "ordered")
  expect_setequal(levels(x), c("low", "mid", "high"))
})

test_that("draw_likert returns values in expected range", {
  x <- draw_likert(rnorm(100), min = -3, max = 3, bins = 5)
  expect_true(all(x %in% 1:5))
})

test_that("draw_normal_icc produces target ICC", {
  set.seed(42)
  clusters <- rep(1:20, each = 50)
  y <- draw_normal_icc(clusters = clusters, ICC = 0.4)
  r2 <- summary(lm(y ~ factor(clusters)))$r.squared
  expect_gt(r2, 0.25)
  expect_lt(r2, 0.60)
})

test_that("draw_binary_icc produces target ICC", {
  set.seed(42)
  clusters <- rep(1:20, each = 50)
  y <- draw_binary_icc(clusters = clusters, prob = 0.5, ICC = 0.3)
  r2 <- summary(lm(y ~ factor(clusters)))$r.squared
  expect_gt(r2, 0.10)  # loose bound given stochasticity
})

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

test_that("split_quantile returns a factor with the right levels", {
  x <- rnorm(100)
  q <- split_quantile(x, type = 4)
  expect_s3_class(q, "factor")
  expect_equal(nlevels(q), 4L)
})

test_that("quantile splits are unordered factors, as in fabricatr", {
  # An ordered factor would make lm() fit polynomial contrasts (q.L, q.Q, q.C)
  # where the same script under fabricatr gets treatment contrasts (q2, q3, q4).
  q <- draw_quantile(type = 4, N = 200)
  expect_s3_class(q, "factor")
  expect_false(is.ordered(q))
  expect_false(is.ordered(split_quantile(rnorm(100), type = 3)))

  set.seed(1)
  fit <- lm(rnorm(200) ~ q)
  expect_equal(names(coef(fit)), c("(Intercept)", "q2", "q3", "q4"))
})

test_that("draw_quantile fills buckets evenly and validates its arguments", {
  expect_equal(unname(as.integer(table(draw_quantile(type = 4, N = 100)))),
               rep(25L, 4))
  expect_equal(levels(draw_quantile(type = 3, N = 30)), c("1", "2", "3"))
  expect_error(draw_quantile(type = 1, N = 20), "between 2 and N-1")
  expect_error(draw_quantile(type = 20, N = 20), "between 2 and N-1")
})

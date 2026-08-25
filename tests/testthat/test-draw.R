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

test_that("draw_categorical with labels returns an unordered factor", {
  # Nominal categories, so unordered, as in fabricatr 1.0.2. An ordered factor
  # would make lm() fit polynomial contrasts for it.
  x <- draw_categorical(prob = c(0.3, 0.4, 0.3), N = 50,
                        labels = c("low", "mid", "high"))
  expect_s3_class(x, "factor")
  expect_false(is.ordered(x))
  expect_equal(levels(x), c("low", "mid", "high"))
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

test_that("draw_normal_icc accepts the endpoints of the ICC range", {
  # fabricatr#149: an ICC of 0 or 1 is degenerate, not an error.
  set.seed(4)
  cl <- rep(1:20, each = 10)

  y0 <- draw_normal_icc(clusters = cl, ICC = 0)
  expect_length(y0, 200L)
  expect_true(is.finite(sd(y0)))

  y1 <- draw_normal_icc(clusters = cl, ICC = 1)
  expect_length(y1, 200L)
  expect_true(is.finite(sd(y1)))
  # every unit in a cluster takes that cluster's value
  expect_equal(length(unique(y1)), 20L)
  expect_true(all(tapply(y1, cl, function(v) length(unique(v))) == 1L))

  expect_error(draw_normal_icc(clusters = cl, ICC = 1.5), "between 0 and 1")
  expect_error(draw_normal_icc(clusters = cl, ICC = -0.1), "between 0 and 1")
})

test_that("cluster-level parameters may arrive already expanded to one per unit", {
  # fabricatr#189: prob defined at the cluster level is length N by the time a
  # nested level evaluates. Indexing it by cluster number read the first k
  # entries and paired the wrong probability with each cluster.
  set.seed(9)
  dat <- fabricate(
    clusters = add_level(N = 30, prob = runif(N, 0, 0.9),
                         prob = ifelse(prob < 0.4, 0, prob)),
    people   = add_level(N = 10)
  )
  y <- draw_binary_icc(prob = dat$prob, clusters = dat$clusters, ICC = 0.1)
  expect_true(all(y[dat$prob == 0] == 0))

  expect_error(
    draw_binary_icc(prob = runif(300), clusters = dat$clusters, ICC = 0.1),
    "constant inside each cluster"
  )
  expect_error(
    draw_binary_icc(prob = runif(7), clusters = dat$clusters, ICC = 0.1),
    "must have length 1"
  )
})

test_that("total_sd parameterises the draw at the ICC endpoints too", {
  set.seed(3)
  cl <- rep(1:20, each = 10)

  # ICC = 0: the cluster variable does no work; total_sd sets the scale
  y0 <- draw_normal_icc(clusters = cl, ICC = 0, total_sd = 2)
  expect_true(all(tapply(y0, cl, function(v) length(unique(v))) == 10L))
  expect_equal(sd(y0), 2, tolerance = 0.25)

  # ICC = 1: every unit takes its cluster's value; total_sd sets the scale
  y1 <- draw_normal_icc(clusters = cl, ICC = 1, total_sd = 2)
  expect_true(all(tapply(y1, cl, function(v) length(unique(v))) == 1L))
  expect_equal(sd(y1), 2, tolerance = 0.6)

  # ICC alone still works at both ends
  expect_length(draw_normal_icc(clusters = cl, ICC = 0), 200L)
  expect_true(all(tapply(draw_normal_icc(clusters = cl, ICC = 1), cl,
                         function(v) length(unique(v))) == 1L))
})

test_that("any two of ICC, sd, sd_between and total_sd pin the same draw", {
  set.seed(5)
  cl <- rep(1:40, each = 10)
  target_total <- 3; target_icc <- 0.5
  w <- target_total * sqrt(1 - target_icc)
  b <- target_total * sqrt(target_icc)

  combos <- list(
    draw_normal_icc(clusters = cl, ICC = target_icc, total_sd = target_total),
    draw_normal_icc(clusters = cl, ICC = target_icc, sd = w),
    draw_normal_icc(clusters = cl, ICC = target_icc, sd_between = b),
    draw_normal_icc(clusters = cl, sd = w, sd_between = b),
    draw_normal_icc(clusters = cl, total_sd = target_total, sd = w),
    draw_normal_icc(clusters = cl, total_sd = target_total, sd_between = b)
  )
  # R-squared on a 40-level factor is upward biased (E[R2] is about 0.55 here,
  # not 0.50), so assert a band around what the DGP actually produces rather
  # than the nominal target.
  for (y in combos) {
    expect_gt(sd(y), 2.4); expect_lt(sd(y), 3.6)
    r2 <- summary(lm(y ~ factor(cl)))$r.squared
    expect_gt(r2, 0.45); expect_lt(r2, 0.70)
  }
})

test_that("over- and under-determined scale specifications are refused", {
  cl <- rep(1:10, each = 10)
  expect_error(draw_normal_icc(clusters = cl, ICC = 0.5, sd = 1, total_sd = 2),
               "only one of")
  expect_error(draw_normal_icc(clusters = cl, total_sd = 1, sd = 2),
               "cannot exceed")
  expect_error(draw_normal_icc(clusters = cl, total_sd = 1, sd_between = 2),
               "cannot exceed")
  expect_error(draw_normal_icc(clusters = cl, sd = 1), "any two of")
  expect_error(draw_normal_icc(clusters = cl, ICC = 1, sd = 1),
               "`sd` must be 0")
  expect_error(draw_normal_icc(clusters = cl, ICC = 0, sd_between = 1),
               "`sd_between` must be 0")
  expect_warning(draw_normal_icc(clusters = cl, ICC = 0.5, sd = 1,
                                 sd_between = 1), "ignoring")
})

test_that("total_sd leaves the realised sd free to vary, unlike fabricatr", {
  # fabricatr#133. fabricatr rescales the finished vector so sd() is exactly
  # total_sd every draw; here total_sd is a parameter, so the realised sd
  # varies as any sample statistic does.
  set.seed(3)
  cl <- rep(1:10, each = 10)
  reps <- replicate(60, sd(draw_normal_icc(clusters = cl, ICC = 0.4,
                                           total_sd = 2)))
  expect_gt(sd(reps), 0.01)
  expect_equal(mean(reps), 2, tolerance = 0.2)
})

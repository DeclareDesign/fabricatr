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

test_that("draw_ordered takes the identity link only, and N cannot recycle", {
  x <- rnorm(10)
  expect_error(draw_ordered(x, breaks = 0, link = "probit"), "identity")
  expect_error(draw_ordered(x, breaks = 0, N = 20), "must equal length\\(x\\)")
  expect_error(draw_ordered(x, breaks = 0, N = 5), "must equal length\\(x\\)")
  expect_length(draw_ordered(x, breaks = 0, N = 10), 10L)
  expect_length(draw_ordered(x, breaks = 0), 10L)
})

test_that("draw_likert returns values in expected range", {
  x <- draw_likert(rnorm(100), min = -3, max = 3, bins = 5)
  expect_true(all(x %in% 1:5))
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

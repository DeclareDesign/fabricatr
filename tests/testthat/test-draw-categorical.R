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

test_that("draw_categorical validates prob and N", {
  expect_error(draw_categorical(prob = c("a", "b"), N = 5), "numeric vector")
  expect_error(draw_categorical(prob = 0.5, N = 5), "length >= 2")
  expect_error(draw_categorical(prob = c(0.5, 0.5)), "Supply `N`")
  expect_error(draw_categorical(prob = matrix(letters[1:4], 2)), "non-negative")
  expect_error(draw_categorical(prob = matrix(c(-1, 2, 1, 1), 2)), "non-negative")
  expect_error(draw_categorical(prob = matrix(c(0.5, 0.5, 0.5, 0.5), 2), N = 3),
               "nrow\\(prob\\) must equal N")
  expect_error(draw_categorical(prob = c(0.5, 0.5), N = 4, labels = c("a", "b", "c")),
               "must equal the number of categories \\(2\\)")
  expect_error(draw_categorical(prob = matrix(c(0, 0, 1, 1), 2, byrow = TRUE)),
               "sums to zero")
})

test_that("an NA in prob or N is named, not left to an internal comparison", {
  # `any(prob < 0, na.rm = TRUE)` is FALSE for an NA, so the NA used to reach
  # `any(row_sums == 0)` and surface as "missing value where TRUE/FALSE needed",
  # which names neither the argument nor the function.
  expect_error(draw_categorical(prob = c(0.5, NA), N = 4), "must not contain NA")
  expect_error(draw_categorical(prob = c(0.5, 0.5), N = NA),
               "`N` must be a single positive integer")
  expect_error(draw_categorical(prob = c(0.5, 0.5), N = 0),
               "`N` must be a single positive integer")
})

test_that("draw_categorical takes N from the matrix when N is not given", {
  x <- draw_categorical(prob = matrix(rep(0.25, 12), nrow = 3))
  expect_length(x, 3L)
  expect_true(all(x %in% 1:4))
})

test_that("draw_ordered validates breaks and labels", {
  x <- rnorm(5)
  expect_error(draw_ordered(x), "Supply numeric `breaks`")
  expect_error(draw_ordered(x, breaks = NULL), "Supply numeric `breaks`")
  expect_error(draw_ordered(x, breaks = c(0, NA)), "Supply numeric `breaks`")
  expect_error(draw_ordered(x, breaks = c("a", "b")), "`breaks` must be numeric")
  expect_error(draw_ordered(x, breaks = c(1, 0)), "ascending order")
  expect_error(draw_ordered(x, breaks = c(-1, 1), labels = c("a", "b")),
               "length\\(breaks\\) \\+ 1 = 3")
})

test_that("draw_ordered(strict = TRUE) makes values outside the breaks NA", {
  # A formal the man page documents three times and no test set, so the branch
  # never ran. Values below the first break or above the last have no bounded
  # category, and strict = TRUE refuses to put them in the open end ones.
  x <- c(-5, -0.5, 5)
  expect_equal(draw_ordered(x, breaks = c(-1, 1), strict = TRUE),
               c(NA, 2L, NA))
  expect_equal(draw_ordered(x, breaks = c(-1, 1)), c(1L, 2L, 3L))
  f <- draw_ordered(x, breaks = c(-1, 1), labels = c("lo", "mid", "hi"),
                    strict = TRUE)
  expect_s3_class(f, "ordered")
  expect_equal(as.character(f), c(NA, "mid", NA))
})

test_that("draw_likert requires breaks or all three of min, max and bins", {
  x <- rnorm(5)
  expect_error(draw_likert(x, max = 3, bins = 5), "Provide either `breaks`")
  expect_error(draw_likert(x, min = -3, bins = 5), "Provide either `breaks`")
  expect_error(draw_likert(x, min = -3, max = 3), "Provide either `breaks`")
  # Present but unusable: these reached seq() and failed as 'from'/'length.out'.
  expect_error(draw_likert(x, min = NA, max = 3, bins = 5), "`min` must be")
  expect_error(draw_likert(x, min = -3, max = NA, bins = 5), "`max` must be")
  expect_error(draw_likert(x, min = -3, max = 3, bins = NA), "`bins` must be")
})

test_that("split_quantile validates x and type", {
  expect_error(split_quantile(1, type = 2), "length >= 2")
  expect_error(split_quantile(rnorm(10), type = 1), "integer >= 2")
  expect_error(split_quantile(rnorm(10), type = "4"), "integer >= 2")
  expect_error(split_quantile(rnorm(10), type = NA_real_), "integer >= 2")
  # A fractional type produced as many cut intervals as labels only by accident,
  # and otherwise died inside cut() on the mismatch.
  expect_error(split_quantile(rnorm(10), type = 2.5), "integer >= 2")
})

test_that("draw_quantile validates N and type", {
  expect_error(draw_quantile(type = 2, N = 0), "single positive integer")
  expect_error(draw_quantile(type = 2, N = "10"), "single positive integer")
  expect_error(draw_quantile(type = 2, N = NA_real_), "single positive integer")
  expect_error(draw_quantile(type = NA_real_, N = 20), "between 2 and N-1")
  expect_error(draw_quantile(type = 2.5, N = 20), "between 2 and N-1")
})

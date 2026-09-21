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

# A link acts on `latent` and on nothing else -----------------------------------
# Each of these returned a silently different answer before: an unrecognised
# name and a function both fell through to the identity, and a `prob` or `mean`
# supplied with a link had the link dropped. 1.0.2 refuses the first two.
test_that("an unrecognised link name is an error, not the identity", {
  lat <- rep(0.3, 20)
  expect_error(draw_binary(latent = lat, link = "logti"), "must be a function")
  expect_error(draw_binary(latent = lat, link = "banana"), "must be a function")
  expect_error(draw_count(latent = lat, link = "lgo"), "must be a function")
  expect_error(draw_binomial(latent = lat, trials = 2, link = NA), "must be a function")
})

test_that("a function link is applied, as in 1.x", {
  lat <- rnorm(500)
  set.seed(1); by_name <- draw_binary(latent = lat, link = "logit")
  set.seed(1); by_fn <- draw_binary(latent = lat, link = plogis)
  expect_identical(by_name, by_fn)
  set.seed(1); by_name <- draw_binary(latent = lat, link = "probit")
  set.seed(1); by_fn <- draw_binary(latent = lat, link = pnorm)
  expect_identical(by_name, by_fn)
})

test_that("prob or mean with a non-identity link is refused", {
  # 0.3 is a legal probability and log(3) a legal rate, so no range check can
  # tell either apart from a latent the author meant to be transformed.
  expect_error(draw_binary(prob = rep(0.3, 20), link = "logit"),
               "acts on `latent`")
  expect_error(draw_binomial(prob = rep(0.3, 20), trials = 2, link = "probit"),
               "acts on `latent`")
  expect_error(draw_count(mean = rep(log(3), 20), link = "log"),
               "acts on `latent`")
  # The spelling that works keeps working.
  expect_length(draw_binary(latent = rnorm(20), link = "logit"), 20L)
  expect_equal(mean(draw_count(latent = rep(log(3), 20000), link = "log")),
               3, tolerance = 0.05)
})

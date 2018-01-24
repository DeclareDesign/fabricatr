context("Variable functions")

test_that("Variable functions", {
  # Single-level data, logit link, inherit or implicit N
  fabricate(my_level = add_level(
    N = 10,
    Y1 = rnorm(N),
    Y2 = draw_binary(Y1, link = "logit")
  ))

  # Single level, count, inherit or implicit N
  fabricate(my_level = add_level(
    N = 10,
    Y1 = rnorm(N, 5),
    Y2 = draw_count(mean = Y1)
  ))
})

test_that("Randomized data is random.", {
  # Verify we're not generating exactly the same data every time
  expect_equal(
    all(
      draw_count(mean = runif(50, 0, 5)) == draw_count(mean = runif(50, 0, 5))
    ), FALSE
  )
})

test_that("Seeded data is non-random.", {
  set.seed(1)
  j <- draw_count(mean = runif(50, 0, 5))
  set.seed(1)
  k <- draw_count(mean = runif(50, 0, 5))
  expect_equal(all(j == k), TRUE)
})

test_that("Binary invalid specification tests", {
  # Binary data, invalid probabilities.
  expect_error(draw_binary(prob = -1, N = 10)) # Negative
  expect_error(draw_binary(prob = c("invalid", "probability"), N = 10)) # Non-numeric
  expect_error(draw_binary(prob = 1.2, N = 10)) # Positive outside 0-1
  expect_error(draw_binary(prob = c(0.5, 0.5, "invalid mixed"), N = 10)) # Mixed non-numeric
  expect_error(draw_binary()) # No arguments
  expect_error(draw_binary(N = 10)) # Missing probability
  expect_error(draw_binary(prob = c(0.3, 0.4, 0.5), N = 10)) # Not a multiple, don't want to recycle
  expect_error(draw_binary(prob = c(0.5, 0.9), type = "binary", N = 10, trials = 2))
  # Invalid trials for binary data
})

test_that("Binary valid tests", {
  # Valid binary data
  draw_binary(prob = c(0.5, 0.9), N = 10)
  # Logit link
  draw_binary(prob = rnorm(5), link = "logit")
  # Probit link
  draw_binary(prob = rnorm(5), link = "probit")
  # Identity link
  draw_binary(prob = runif(5, 0, 1), link = "identity")
  # Draw binary, implicit N
  draw_binary(prob = runif(100))
})

test_that("Binomial invalid tests", {
  # Binomial data, invalid probabilities
  expect_error(draw_binomial(prob = -1, N = 10)) # Negative
  expect_error(draw_binomial(prob = c("invalid", "probability"), N = 10)) # Non-numeric
  expect_error(draw_binomial(prob = 1.2, N = 10)) # Positive outside 0-1
  expect_error(draw_binomial(prob = c(0.5, 0.5, "invalid mixed"), N = 10)) # Mixed non-numeric
  expect_error(draw_binomial()) # No arguments
  expect_error(draw_binomial(prob = 0.3, N = 10, trials = 2.5)) # Non-integer trials
  expect_error(draw_binomial(prob = 0.3, N = 10, trials = c(2.5, 3))) # Non-integer trials, mixed trials num.

  expect_error(draw_binomial(N = 10)) # Missing probability
  expect_error(draw_binomial(prob = c(0.3, 0.4, 0.5), N = 10)) # Not a multiple, don't want to recycle

  # Binomial data, invalid k
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = NA)) # NA
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = "invalid")) # Character
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = 0.5)) # Non-integer
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = c(1, 0.5))) # Non-integer mixed
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = -1)) # Negative integer
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = c(10, 100, 1000))) # Non-multiple
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = c(10, "mixed invalid"))) # Mixed non-integer
  expect_error(draw_binomial(prob = 0.5, N = 10, trials = matrix(NA, ncol = 3, nrow = 3))) # Higher dim k
})

test_that("Binomial valid tests", {
  # Binomial data, same trials
  draw_binomial(prob = c(0.2, 0.8), trials = 10)
  # Binomial data, different trials for each obs.
  draw_binomial(prob = c(0.2, 0.8), trials = c(10, 100))
  # Binomial data, no trials specified (should default to 1)
  draw_binomial(prob = c(0.2, 0.8))

  # Valid binomial draw
  set.seed(1)
  valid_draw <- draw_binomial(prob = 0.5, trials = 10, N = 25)
  expect_gte(mean(valid_draw), 4)
  expect_lte(mean(valid_draw), 6)
})

test_that("Invalid link", {
  expect_error(draw_binary(prob = rnorm(5), link = "link-that-doesn't-exist"))
  expect_error(draw_count(mean = rnorm(5), link = "link-that-doesn't-exist"))
})

test_that("Count invalid tests", {
  expect_error(draw_count(mean = 1, N = 5, link = "logit")) # Links are not allowed
  expect_error(draw_count(mean = "invalid", N = 5)) # Invalid lambda
  expect_error(draw_count(mean = -1, N = 5)) # Invalid lambda, negative
  expect_error(draw_count(mean = c(1, 2, 3, 4, -1), N = 5)) # Mixed lambdas, one negative
  expect_error(draw_count(mean = c(1, 2, 3, 4, "invalid"), N = 5)) # Mixed lambdas, one character
  expect_error(draw_count(mean = c(1, 5, 7), N = 2))
})

test_that("Count valid tests", {
  # Base case
  draw_count(mean = 5, N = 25)

  # Draw count, implicit N
  draw_count(mean = runif(100))

  # Count data, multiple means
  draw_count(mean = runif(5, 0, 5))
})

test_that("Categorical invalid tests", {
  expect_error(draw_categorical(
    prob = c(-1, 0, -0.5),
    N = 3
  )) # Negative probability
  expect_error(draw_categorical(prob = "invalid", N = 3)) # Non-numeric probability
  expect_error(draw_categorical(prob = 0.3, N = 3)) # Only one class label
  expect_error(draw_categorical(
    prob = c(0.5, 0.75),
    N = 10, link = "probit"
  )) # Link functions not accepted
  expect_error(draw_categorical(prob = c(0.3, 0.3, 0.4)))
  expect_error(draw_categorical(
    prob =
      matrix(rep(c(0.3, 0.3, 0.4), 3), byrow = TRUE, ncol = 3),
    N = 4
  ))
})

test_that("Categorical valid tests", {
  draw_categorical(prob = matrix(
    rep(c(0.3, 0.3, 0.4), 3),
    byrow = TRUE, ncol = 3, nrow = 3
  ))

  # Convert vector of probabilities to matrix of probabilities
  expect_warning(draw_categorical(prob = c(0.3, 0.3, 0.4), N = 3))
})

test_that("Ordered data invalid tests", {
  expect_error(draw_ordered(
    x = rnorm(5),
    breaks = NA,
    break_labels = NA
  )) # Need to specify breaks
  expect_error(draw_ordered(
    x = rnorm(5),
    breaks = c("invalid", "break", "test"),
    break_labels = NA
  )) # Non-numeric breaks
  expect_error(draw_ordered(
    x = rnorm(5),
    breaks = c(1, 3, 2),
    break_labels = NA
  )) # Breaks out of order
  expect_error(draw_ordered(
    x = rnorm(5),
    breaks = matrix(
      rep(c(0, 1, 2), 3),
      byrow = TRUE, ncol = 3, nrow = 3
    )
  )) # Non-vector breaks
  expect_error(draw_ordered(
    x = rnorm(5),
    breaks = c(-Inf, 0, Inf),
    break_labels = c(1)
  )) # Invalid length break labels.
  expect_error(draw_ordered(
    x = rnorm(5),
    breaks = c(-Inf, 0, Inf),
    link = "logit"
  ))
  expect_error(draw_ordered(
    x = rnorm(5),
    N = 3,
    breaks = c(-Inf, 0, Inf)
  ))
})

test_that("Ordered data valid tests", {
  draw_ordered(
    rnorm(5),
    breaks = c(-Inf, -1, 0, 1, Inf),
    break_labels = c("A", "B", "C", "D")
  )

  # Probit link
  draw_ordered(
    rnorm(5),
    breaks = c(-Inf, 0, Inf),
    break_labels = c("A", "B"),
    link = "probit"
  )
})

test_that("Binary ICCs", {
  clusters <- rep(1:5, 10)
  # Single probability
  draw_binary_icc(clusters = clusters)
  # Probability = length(cluster ids)
  draw_binary_icc(prob = c(0.3, 0.5, 0.7, 0.8, 0.9), clusters = clusters)

  # No clusters at all.
  expect_error(draw_binary_icc(clusters = NULL))
  # Invalid cluster IDs: dimensional
  expect_error(draw_binary_icc(clusters = data.frame(X = 1:10, Y = 1:10)))
  # Invalid cluster IDs: mixed list
  expect_error(draw_binary_icc(clusters = list("abc", 7)))
  # X doesn't match cluster IDs
  expect_error(draw_binary_icc(prob = c(0.5, 0.8), clusters = clusters))
  # X isn't a vector
  expect_error(draw_binary_icc(
    prob = data.frame(
      j = c(0.1, 0.2),
      k = c(0.2, 0.4),
      m = c(0.3, 0.6),
      o = c(0.4, 0.8),
      p = c(0.5, 1.0)
    ),
    clusters = clusters,
    N = length(clusters)
  ))
  # prob isn't numeric
  expect_error(draw_binary_icc(prob = "hello", clusters = clusters))
  # prob isn't a probability
  expect_error(draw_binary_icc(prob = -0.5, clusters = clusters))
  # ICC isn't a single number
  expect_error(draw_binary_icc(clusters = clusters, ICC = c(0.5, 0.8)))
  # ICC isn't a probability
  expect_error(draw_binary_icc(clusters = clusters, ICC = 2))
  # ICC isn't a number
  expect_error(draw_binary_icc(clusters = clusters, ICC = "hello"))
  # Non-numeric N
  expect_error(draw_binary_icc(clusters = clusters, N = "hello"))
  # N provided but doesn't match
  expect_error(draw_binary_icc(clusters = clusters, N = 20))
  # length(x) == N, but cluster mean is not unique by cluster
  clusters <- rep(1:10, 10)
  cluster_means <- sample(rep(seq(0.1, 1, 0.1), 10))
  expect_error(draw_binary_icc(prob = cluster_means, clusters = clusters))
})

test_that("Likert data example using ordered", {
  set.seed(19861108)
  latent <- rnorm(n = 100, mean = 3, sd = 10)
  cutpoints <- c(-15, -7, -3, 3, 7, 15)
  likert <- draw_ordered(
    x = latent,
    breaks = cutpoints
  )
  expect_equal(length(unique(likert)), 7)
  expect_equal(max(as.numeric(likert)), 7)
  expect_equal(min(as.numeric(likert)), 1)

  draw_ordered(
    x = latent,
    breaks = cutpoints,
    break_labels = c(
      "Strongly Disagree",
      "Disagree",
      "Lean Disagree",
      "No Opinion",
      "Lean Agree",
      "Agree",
      "Strongly Agree"
    )
  )
})

test_that("Normal ICC", {
  clusters <- rep(1:5, 10)
  # length(mean) = length(cluster ids)
  draw_normal_icc(mean = c(-1, -0.5, 0, 0.5, 1), clusters = clusters, ICC = 0.5)
  # length(mean) = 1
  draw_normal_icc(mean = 0, clusters = clusters, ICC = 0.5)

  # Don't provide ICC, provide the other two
  draw_normal_icc(clusters = clusters, sd = 1, sd_between = 10)
  # Don't provide ICC, don't provide the other two
  expect_error(draw_normal_icc(clusters = clusters, sd = 1))
  # Provide ICC and sd_between
  draw_normal_icc(clusters = clusters, sd_between = 1, ICC = 0.5)

  # ICCs that hit edge cases
  expect_error(draw_normal_icc(clusters = clusters, ICC = 1))
  expect_error(draw_normal_icc(clusters = clusters, ICC = 0))
  expect_error(draw_normal_icc(clusters = clusters, ICC = 1, sd_between = 1))
  expect_error(draw_normal_icc(clusters = clusters, ICC = 0, sd_between = 1))

  # Provided all three, how can they possibly agree?
  expect_warning(draw_normal_icc(clusters = clusters, ICC = 0.5, sd = 1, sd_between = 1))

  # Invalid cluster IDs: dimensional
  expect_error(draw_normal_icc(clusters = data.frame(X = 1:10, Y = 1:10)))
  # Invalid cluster IDs: mixed list
  expect_error(draw_normal_icc(clusters = list("abc", 7)))
  # X doesn't match cluster IDs
  expect_error(draw_normal_icc(mean = c(0.5, 0.8), clusters = clusters, ICC = 0.5))
  # X isn't a vector
  expect_error(draw_normal_icc(
    mean = data.frame(
      j = c(0.1, 0.2),
      k = c(0.2, 0.4),
      m = c(0.3, 0.6),
      o = c(0.4, 0.8),
      p = c(0.5, 1.0)
    ),
    clusters = clusters,
    ICC = 0.5
  ))
  # mean isn't numeric
  expect_error(draw_normal_icc(mean = "hello", clusters = clusters, ICC = 0.5))
  # ICC isn't a single number
  expect_error(draw_normal_icc(clusters = clusters, ICC = c(0.5, 0.8)))
  # ICC isn't a 0-1 proportion.
  expect_error(draw_normal_icc(clusters = clusters, ICC = 2))
  # ICC isn't a number
  expect_error(draw_normal_icc(clusters = clusters, ICC = "hello"))
  # Non-numeric N
  expect_error(draw_normal_icc(clusters = clusters, N = "hello", ICC = 0.5))
  # N provided but doesn't match
  expect_error(draw_normal_icc(clusters = clusters, N = 20, ICC = 0.5))
  # SD is wrong length
  expect_error(draw_normal_icc(clusters = clusters, sd = c(1, 2), ICC = 0.5))
  # SD is negative
  expect_error(draw_normal_icc(clusters = clusters, sd = -1, ICC = 0.5))
  # SD is non-numeric
  expect_error(draw_normal_icc(clusters = clusters, sd = "hello", ICC = 0.5))
  # SD is not a vector
  expect_error(draw_normal_icc(
    sd = data.frame(
      j = c(0.1, 0.2),
      k = c(0.2, 0.4),
      m = c(0.3, 0.6),
      o = c(0.4, 0.8),
      p = c(0.5, 1.0)
    ),
    clusters = clusters,
    ICC = 0.5
  ))

  # sd_between is wrong length
  expect_error(draw_normal_icc(clusters = clusters, sd_between = c(1, 2), ICC = 0.5))
  # sd_between is negative
  expect_error(draw_normal_icc(clusters = clusters, sd_between = -1, ICC = 0.5))
  # sd_between is non-numeric
  expect_error(draw_normal_icc(clusters = clusters, sd_between = "hello", ICC = 0.5))
  # sd_between is not a vector
  expect_error(draw_normal_icc(
    sd_between = data.frame(
      j = c(0.1, 0.2),
      k = c(0.2, 0.4),
      m = c(0.3, 0.6),
      o = c(0.4, 0.8),
      p = c(0.5, 1.0)
    ),
    clusters = clusters,
    ICC = 0.5
  ))

  # Provide valid SD and SD_between but they're not the same length.
  expect_error(draw_normal_icc(
    sd = 1,
    sd_between = seq(0.5, 2.5, 0.5),
    clusters = clusters
  ))

  # length(mean) == N, but mean is non-unique per cluster
  clusters <- rep(1:10, 10)
  cluster_means <- sample(rep(1:10, 10))
  expect_error(draw_normal_icc(mean = cluster_means, clusters = clusters, ICC = 0.5))
})

test_that("Likert alias", {
  # Without specifying anything
  draw_likert(x = rnorm(100))

  # Specifying breaks
  draw_likert(x = rnorm(100), breaks = c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf))
  draw_likert(x = rnorm(100), breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf))
  draw_likert(x = rnorm(100), breaks = c(-Inf, -1, 0, 1, Inf))

  # Specifying types
  draw_likert(x = rnorm(100), type = 7)
  draw_likert(x = rnorm(100), type = 5)
  draw_likert(x = rnorm(100), type = 4)

  # Errors: bad breaks, bad types
  expect_error(draw_likert(x = rnorm(100), breaks = c(-Inf, -1, 0, Inf)))
  expect_error(draw_likert(x = rnorm(100), type = 3))

  # Should be impossible, but verify if someone accidentally overrides type.
  # and doesn't provide breaks.
  expect_error(draw_likert(x = rnorm(100), type = NULL))
})

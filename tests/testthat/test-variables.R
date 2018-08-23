context("Variable functions")

test_that("Variable functions", {
  # Single-level data, logit link, inherit or implicit N
  set.seed(19861108)
  check_binary_mean <- fabricate(my_level = add_level(
    N = 1000,
    Y1 = rnorm(N),
    Y2 = draw_binary(latent = Y1, link = "logit")
  ))
  implied_prob <- 1 / (1 + exp(-check_binary_mean$Y1))
  expect_gte(cor(implied_prob, check_binary_mean$Y2), 0.4)

  # Single level, count, inherit or implicit N
  set.seed(19861108)
  check_count_mean <- fabricate(my_level = add_level(
    N = 1000,
    Y1 = rnorm(N, 5),
    Y2 = draw_count(mean = Y1)
  ))
  model_check_fit <- lm(Y2 ~ Y1, data = check_count_mean)
  expect_gte(model_check_fit$coefficients[2], 0.9)
  expect_lte(model_check_fit$coefficients[2], 1.1)
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
  # Negative
  expect_error(draw_binary(prob = -1, N = 10))
  # Non-numeric
  expect_error(draw_binary(prob = c("invalid", "probability"), N = 10))
  # Positive outside 0-1
  expect_error(draw_binary(prob = 1.2, N = 10))
  # Mixed non-numeric
  expect_error(draw_binary(prob = c(0.5, 0.5, "invalid mixed"), N = 10))
  # No arguments
  expect_error(draw_binary())
  # Missing probability
  expect_error(draw_binary(N = 10))
  # Not a multiple, don't want to recycle
  expect_error(draw_binary(prob = c(0.3, 0.4, 0.5), N = 10))
  # Invalid trials for binary data
  expect_error(draw_binary(prob = c(0.5, 0.9),
                           type = "binary", N = 10, trials = 2))
})

test_that("Binary valid tests", {
  # Valid binary data
  basic_binary <- draw_binary(prob = c(0.5, 0.9), N = 10)
  expect_equal(length(basic_binary), 10)
  # Logit link
  draw_binary(latent = rnorm(5), link = "logit")
  # Probit link
  draw_binary(latent = rnorm(5), link = "probit")
  # Identity link
  draw_binary(latent = runif(5, 0, 1), link = "identity")
  # Draw binary, implicit N
  draw_binary(prob = runif(100))

})

test_that("Binomial invalid tests", {
  # Binomial data, invalid probabilities
  # Negative
  expect_error(draw_binomial(prob = -1, N = 10))
  # Non-numeric
  expect_error(draw_binomial(prob = c("invalid", "probability"), N = 10))
  # Positive outside 0-1
  expect_error(draw_binomial(prob = 1.2, N = 10))
  # Mixed non-numeric
  expect_error(draw_binomial(prob = c(0.5, 0.5, "invalid mixed"), N = 10))
  # No arguments
  expect_error(draw_binomial())
  # Non-integer trials
  expect_error(draw_binomial(prob = 0.3, N = 10, trials = 2.5))
  # Non-integer trials, mixed trials num.
  expect_error(draw_binomial(prob = 0.3, N = 10, trials = c(2.5, 3)))
  # Missing probability
  expect_error(draw_binomial(N = 10))
  # Not a multiple, don't want to recycle
  expect_error(draw_binomial(prob = c(0.3, 0.4, 0.5), N = 10))

  # Binomial data, invalid k
  # NA
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = NA))
  # Character
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = "invalid"))
  # Non-integer
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = 0.5))
  # Non-integer mixed
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = c(1, 0.5)))
  # Negative integer
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = -1))
  # Non-multiple
  expect_error(draw_binomial(prob = c(0.2, 0.8), trials = c(10, 100, 1000)))
  # Mixed non-integer
  expect_error(draw_binomial(prob = c(0.2, 0.8),
                             trials = c(10, "mixed invalid")))
  # Higher dim k
  expect_error(draw_binomial(prob = 0.5,
                             N = 10,
                             trials = matrix(NA, ncol = 3, nrow = 3)))
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
  # Links are not allowed
  expect_error(draw_count(mean = 1, N = 5, link = "logit"))
  # Invalid lambda
  expect_error(draw_count(mean = "invalid", N = 5))
  # Invalid lambda, negative
  expect_error(draw_count(mean = -1, N = 5))
  # Mixed lambdas, one negative
  expect_error(draw_count(mean = c(1, 2, 3, 4, -1), N = 5))
  # Mixed lambdas, one character
  expect_error(draw_count(mean = c(1, 2, 3, 4, "invalid"), N = 5))
  expect_error(draw_count(mean = c(1, 5, 7), N = 2))
})

test_that("Count valid tests", {
  # Base case
  set.seed(19861108)
  count_draw <- draw_count(mean = 5, N = 250)
  expect_gte(mean(count_draw), 4)
  expect_lte(mean(count_draw), 6)

  # Draw count, implicit N
  count_draw_implicit_n <- draw_count(mean = runif(100))
  expect_equal(length(count_draw_implicit_n), 100)

  # Count data, multiple means
  draw_count(mean = runif(5, 0, 5))
})

test_that("Categorical invalid tests", {
  expect_error(suppressWarnings(
    draw_categorical(prob = c(-1, 0, -0.5), N = 3)
  )) # Negative probability
  # Non-numeric probability
  expect_error(draw_categorical(prob = "invalid", N = 3))
  # Only one class label
  expect_error(draw_categorical(prob = 0.3, N = 3))
  # Link functions not accepted
  expect_error(draw_categorical(
    prob = c(0.5, 0.75),
    N = 10, link = "probit"
  ))
  expect_error(draw_categorical(prob = c(0.3, 0.3, 0.4)))
  expect_error(draw_categorical(
    prob =
      matrix(rep(c(0.3, 0.3, 0.4), 3), byrow = TRUE, ncol = 3),
    N = 4
  ))

  # Wrong number of labels if specified.
  expect_error(draw_categorical(prob = c(0.3, 0.3, 0.4),
                                category_labels = c("A", "B"),
                                N = 10))
})

test_that("Categorical valid tests", {
  first <- draw_categorical(prob = matrix(
    rep(c(0, 1, 0), 3),
    byrow = TRUE, ncol = 3, nrow = 3
  ))

  expect_equal(first, c(2,2,2))

  second <- draw_categorical(prob = matrix(
      rep(c(0, 1, 0), 3),
      byrow = TRUE, ncol = 3, nrow = 3
    ),category_labels = c("A", "B", "C"))

  expect_equal(second, c("B","B","B"))
  # Convert vector of probabilities to matrix of probabilities
  # Sunset as per #121, leaving deprecated test.
  #expect_message(draw_categorical(prob = c(0.3, 0.3, 0.4), N = 3))
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
  base_ordered <- draw_ordered(
    rnorm(200),
    breaks = c(-Inf, -1, 0, 1, Inf),
    break_labels = c("A", "B", "C", "D")
  )
  expect_equal(length(base_ordered), 200)
  expect_equal(length(table(base_ordered)), 4)
})


test_that("MH's tests",{
  expect_equal(
    draw_ordered(c(-1, .5, .5, .5, 5), breaks = c(1/3, 2/3)),
    c(1, 2, 2, 2, 3))

  expect_equal(
    draw_ordered(c(.3, .5, .5), breaks = c(1/3, 2/3), strict = TRUE),
    c(NA, 1, 1))

  expect_equal(
    draw_ordered(c(.3, .5, .5), breaks = c(1/3, 2/3)),
    c(1, 2, 2))

  expect_equal(
    draw_ordered(c(.5, .5, .7), breaks = c(1/3, 2/3)),
    c(2, 2, 3)
  )

  expect_equal(
    draw_ordered(c(.5, .5, .7), breaks = c(1/3, 2/3), strict = TRUE),
    c(1, 1, NA)
  )

  # now try with manual Inf's
  expect_equal(
    draw_ordered(c(.5, .5, 2/3, 1), breaks = c(-Inf, 2/3), strict = TRUE),
    c(1, 1, 2, NA)
  )

  expect_equal(
    draw_ordered(c(.5, .5, 2/3, 1), breaks = c(-Inf, 2/3)),
    c(1, 1, 2, 2)
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
  draw_normal_icc(mean = c(-1, -0.5, 0, 0.5, 1),
                  clusters = clusters, ICC = 0.5)
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
  expect_warning(draw_normal_icc(clusters = clusters,
                                 ICC = 0.5, sd = 1, sd_between = 1))

  # Invalid cluster IDs: dimensional
  expect_error(draw_normal_icc(clusters = data.frame(X = 1:10, Y = 1:10)))
  # Invalid cluster IDs: mixed list
  expect_error(draw_normal_icc(clusters = list("abc", 7)))
  # X doesn't match cluster IDs
  expect_error(draw_normal_icc(mean = c(0.5, 0.8),
                               clusters = clusters, ICC = 0.5))
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
  expect_error(draw_normal_icc(clusters = clusters,
                               sd_between = c(1, 2), ICC = 0.5))
  # sd_between is negative
  expect_error(draw_normal_icc(clusters = clusters,
                               sd_between = -1, ICC = 0.5))
  # sd_between is non-numeric
  expect_error(draw_normal_icc(clusters = clusters,
                               sd_between = "hello", ICC = 0.5))
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
  expect_error(draw_normal_icc(mean = cluster_means,
                               clusters = clusters, ICC = 0.5))

  # Confirm total_sd works:
  result <- draw_normal_icc(mean = 10, clusters = clusters, ICC = 0.5,
                           total_sd = 10)
  expect_equal(sd(result), 10)

  # And check that it can't be provided without its other helpers:
  expect_error(draw_normal_icc(clusters = clusters, total_sd = 10))
  expect_error(draw_normal_icc(clusters = clusters, total_sd = 10, ICC = 0.5,
                               sd = 10))
  expect_error(draw_normal_icc(clusters = clusters, total_sd = 10, ICC = 0.5,
                               sd_between = 10))
  expect_error(draw_normal_icc(clusters = clusters, ICC = 0.5, total_sd = -1))
  expect_error(draw_normal_icc(clusters = clusters, ICC = 0.5,
                               total_sd = "hello"))
  expect_error(draw_normal_icc(clusters = clusters, ICC = 0.5,
                               total_sd = c(1, 2)))
})

test_that("Likert alias", {
  # Without specifying anything
  draw_likert(x = rnorm(100))

  # Specifying breaks
  draw_likert(x = rnorm(100),
              breaks = c(-Inf, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, Inf))
  draw_likert(x = rnorm(100),
              breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf))
  draw_likert(x = rnorm(100),
              breaks = c(-Inf, -1, 0, 1, Inf))

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

test_that("Quantile and quantile split", {
  # Null N
  expect_error(draw_quantile(type = 4, N = NULL))
  # Non-numeric N
  expect_error(draw_quantile(type = 4, N = "hello"))
  # N of length
  expect_error(draw_quantile(type = 4, N = c(1, 2, 3)))
  # Negative N
  expect_error(draw_quantile(type = 4, N = -1))
  # Null type
  expect_error(draw_quantile(type = NULL, N = 100))
  # Non-numeric type
  expect_error(draw_quantile(type = "hello", N = 100))
  # type of length
  expect_error(draw_quantile(type = c(1, 2), N = 100))
  # Type 0 or negative
  expect_error(draw_quantile(type = -1, N = 100))
  # Type too high
  expect_error(draw_quantile(type = 200, N = 100))

  # Valid draw
  quantile_draws <- draw_quantile(type = 5, N = 100)
  expect_equal(all(table(quantile_draws) == 20), TRUE)

  # Draw of some data to quantile split
  z <- rnorm(n = 100)
  # Null X
  expect_error(split_quantile(x = NULL, type = 4))
  # Null type
  expect_error(split_quantile(x = z, type = NULL))
  # Non-numeric type
  expect_error(split_quantile(x = z, type = "hello"))
  # Single x
  expect_error(split_quantile(x = 4, type = 4))

  split_quantile_data <- split_quantile(x = z, type = 5)
  expect_equal(all(table(split_quantile_data) == 20), TRUE)
})

test_that("Correlated variable draws", {
  # Single base X
  base_dist <- runif(n = 100, min = 50, max = 125)

  # Errors for rho:
  expect_error(correlate(draw_binary, prob = 0.7, given = base_dist)) # No rho
  # Non-numeric rho
  expect_error(correlate(draw_binary, prob = 0.7, given = base_dist, rho = "H"))
  # Rho is more than a number
  expect_error(correlate(draw_binary, prob = 0.7, given = base_dist,
                         rho = c(0.5, -0.2)))
  expect_error(correlate(draw_binary, prob = 0.7, given = base_dist,
                         rho = -2))


  # Errors for given:
  expect_error(correlate(draw_binary, prob = 0.5, given = NULL, rho = 0.5))
  base_dist_df <- data.frame(x = base_dist)
  expect_error(correlate(draw_binary, prob = 0.5,
                         given = base_dist_df, rho = 0.5))

  # Didn't pass a draw_handler:
  expect_error(correlate(NULL, given = base_dist, rho = 0.5))
  expect_error(correlate(base_dist, given = base_dist, rho = 0.5))

  # Now, let's see a working example
  set.seed(19861108)
  count_y <- correlate(draw_count, mean = 50, given = base_dist, rho = 0.5)
  observed_correlation <- cor(count_y, base_dist, method="spearman")
  expect_gte(observed_correlation, 0.4)
  expect_lte(observed_correlation, 0.6)
})

test_that("Correlated variable draws and our distributions", {
  set.seed(19861108)
  base_dist <- draw_count(mean = 50, N = 100)

  # Working binary
  corr_binary <- correlate(draw_binary, prob = 0.5,
                           given = base_dist, rho = 0.5)
  expect_gte(cor(base_dist, corr_binary, method="spearman"), 0.1)
  expect_lte(cor(base_dist, corr_binary, method="spearman"), 0.9)

  # Error handling for binomial
  expect_error(correlate(draw_binomial, prob = rep(0.7, 100), trials = 10,
                         given = base_dist, rho = 0.5))

  # Working binomial
  corr_binomial <- correlate(draw_binomial, prob = 0.5, trials = 10,
                             given = base_dist, rho = 0.5)
  expect_gte(cor(base_dist, corr_binomial, method="spearman"), 0.4)
  expect_lte(cor(base_dist, corr_binomial, method="spearman"), 0.6)

  # Error handling for count
  expect_error(correlate(draw_count, mean = rep(20, 100),
                         given = base_dist, rho = 0.5))

  # Working count
  corr_count <- correlate(draw_count, mean = 20,
                          given = base_dist, rho = 0.5)
  expect_gte(cor(base_dist, corr_count, method="spearman"), 0.4)
  expect_lte(cor(base_dist, corr_count, method="spearman"), 0.6)

  # Using a base R function
  corr_norm <- correlate(rnorm, mean = 20, sd = 5, given = base_dist, rho = 0.5)
  expect_gte(cor(base_dist, corr_norm, method="spearman"), 0.4)
  expect_lte(cor(base_dist, corr_norm, method="spearman"), 0.6)

  # And again
  corr_norm_2 <- correlate(qnorm, mean = 20, sd = 5,
                           given = base_dist, rho = 0.5)
  expect_gte(cor(base_dist, corr_norm_2, method="spearman"), 0.35)
  expect_lte(cor(base_dist, corr_norm_2, method="spearman"), 0.65)


  # Using a poorly specified function
  expect_error(correlate(print, given = base_dist, rho = 0.5))
})

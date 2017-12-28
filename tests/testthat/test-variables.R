context("Variable functions")

test_that("Variable functions", {
  skip("Rewriting draw functions")
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
    Y2 = draw_discrete(Y1, type = "count", k = 3)
  ))
})

test_that("Randomized data is random.", {
  skip("Rewriting draw functions")
  # Verify we're not generating exactly the same data every time
  expect_equal(
    all(
      draw_discrete(runif(50, 0, 5), type="count", k = 5) == draw_discrete(runif(50, 0, 5), type="count", k = 5)
    ), FALSE)
})

test_that("Seeded data is non-random.", {
  skip("Rewriting draw functions")
  set.seed(1)
  j <- draw_discrete(runif(50, 0, 5), type="count", k = 5)
  set.seed(1)
  k <- draw_discrete(runif(50, 0, 5), type="count", k = 5)
  expect_equal(all(j == k), TRUE)
})

test_that("Binary invalid specification tests", {
  skip("Rewriting draw functions")
  # Binary data, invalid probabilities.
  expect_error(draw_binary(x=-1, N=10)) # Negative
  expect_error(draw_binary(x=c("invalid", "probability"), N=10)) # Non-numeric
  expect_error(draw_binary(x=1.2, N=10)) # Positive outside 0-1
  expect_error(draw_binary(x=c(0.5, 0.5, "invalid mixed"), N=10)) # Mixed non-numeric
  expect_error(draw_binary()) # No arguments
  expect_error(draw_binary(N=10)) # Missing probability
  expect_error(draw_binary(x=c(0.3, 0.4, 0.5), N=10)) # Not a multiple, don't want to recycle
  expect_warning(draw_discrete(x=c(0.5, 0.9), type="binary", N=10, k=2)) # Invalid k for binary data
})

test_that("Binary valid tests", {
  skip("Rewriting draw functions")
  # Valid binary data
  draw_binary(x=c(0.5, 0.9), N=10)
  # Logit link
  draw_binary(rnorm(5), link = "logit")
  # Probit link
  draw_binary(rnorm(5), link = "probit")
  # Identity link
  draw_binary(runif(5, 0, 1), link = "identity")
  # Draw binary, implicit N
  draw_binary(runif(100))
})

test_that("Binomial invalid tests", {
  skip("Rewriting draw functions")
  # Binomial data, invalid probabilities
  expect_error(draw_discrete(x=-1, N=10, type="binomial")) # Negative
  expect_error(draw_discrete(x=c("invalid", "probability"), N=10, type="binomial")) # Non-numeric
  expect_error(draw_discrete(x=1.2, N=10, type="binomial")) # Positive outside 0-1
  expect_error(draw_discrete(x=c(0.5, 0.5, "invalid mixed"), N=10, type="binomial")) # Mixed non-numeric
  expect_error(draw_discrete()) # No arguments
  expect_error(draw_discrete(x=0.3, N=10, k=2.5, type="binomial")) # Non-integer k
  expect_error(draw_discrete(x=0.3, N=10, k=c(2.5, 3), type="binomial")) # Non-integer k, mixed trials num.

  expect_error(draw_discrete(N=10, type="binomial")) # Missing probability
  expect_error(draw_discrete(x=c(0.3, 0.4, 0.5), N=10, type="binomial")) # Not a multiple, don't want to recycle

  # Binomial data, invalid k
  expect_error(draw_discrete(x=c(0.2, 0.8), k=NA, type="binomial")) # NA
  expect_error(draw_discrete(x=c(0.2, 0.8), k="invalid", type="binomial")) # Character
  expect_error(draw_discrete(x=c(0.2, 0.8), k=0.5, type="binomial")) # Non-integer
  expect_error(draw_discrete(x=c(0.2, 0.8), k=c(1, 0.5), type="binomial")) # Non-integer mixed
  expect_error(draw_discrete(x=c(0.2, 0.8), k=-1, type="binomial")) # Negative integer
  expect_error(draw_discrete(x=c(0.2, 0.8), k=c(10, 100, 1000), type="binomial")) # Non-multiple
  expect_error(draw_discrete(x=c(0.2, 0.8), k=c(10, "mixed invalid"), type="binomial")) # Mixed non-integer
  expect_error(draw_discrete(x=0.5, N=10, k=matrix(NA, ncol=3, nrow=3), type="binomial")) # Higher dim k
})

test_that("Binomial valid tests", {
  skip("Rewriting draw functions")
  # Binomial data, same k
  draw_discrete(x=c(0.2, 0.8), k=10, type="binomial")
  # Binomial data, different k for each obs.
  draw_discrete(x=c(0.2, 0.8), k=c(10, 100), type="binomial")
  # Binomial data, no k specified (should default to 1)
  draw_discrete(x=c(0.2, 0.8), type="binomial")

  # Valid binomial draw
  draw_discrete(x=0.5, k=10, N=25, type="binomial")
})

test_that("Invalid link", {
  skip("Rewriting draw functions")
  expect_error(draw_binary(rnorm(5), link = "link-that-doesn't-exist"))
  expect_error(draw_count(rnorm(5), k = 5, link = "link-that-doesn't-exist"))
})

test_that("Count invalid tests", {
  skip("Rewriting draw functions")
  expect_error(draw_discrete(x=1, N=5, type="count", link="logit")) # Links are not allowed
  expect_error(draw_discrete(x="invalid", N=5, type="count")) # Invalid lambda
  expect_error(draw_discrete(x=-1, N=5, type="count")) # Invalid lambda, negative
  expect_error(draw_discrete(x=c(1, 2, 3, 4, -1), N=5, type="count")) # Mixed lambdas, one negative
  expect_error(draw_discrete(x=c(1, 2, 3, 4, "invalid"), N=5, type="count")) # Mixed lambdas, one character

})

test_that("Count valid tests", {
  skip("Rewriting draw functions")
  draw_discrete(x=5, N=25, type="count")

  # Draw count, implicit N
  draw_discrete(runif(100), type = "count", k = 4)

  # Count data
  draw_discrete(runif(5, 0, 5), type = "count", k = 5)
  draw_discrete(runif(5, 0, 1), type = "count", k = 5)
})

test_that("Categorical invalid tests", {
  skip("Rewriting draw functions")
  expect_error(draw_discrete(x=c(-1, 0, -0.5), N=3, type="categorical")) # Negative probability
  expect_error(draw_discrete(x="invalid", N=3, type="categorical")) # Non-numeric probability
  expect_error(draw_discrete(x=0.3, N=3, type="categorical")) # Only one class label
  expect_error(draw_discrete(x=c(0.5, 0.75),
                             N=10, type="categorical", link="probit")) # Link functions not accepted
})

test_that("Categorical valid tests", {
  skip("Rewriting draw functions")
  draw_discrete(x=matrix(rep(c(0.3, 0.3, 0.4), 3), byrow=TRUE, ncol=3, nrow=3),
                N=3, type="categorical")

  # Convert vector of probabilities to matrix of probabilities
  expect_warning(draw_discrete(x=c(0.3, 0.3, 0.4), N=3, type="categorical"))
})

test_that("Ordered data invalid tests", {
  skip("Rewriting draw functions")
  expect_error(draw_discrete(x=rnorm(5), type="ordered",
                             breaks=NA, break_labels=NA)) # Need to specify breaks
  expect_error(draw_discrete(x=rnorm(5), type="ordered",
                             breaks=c("invalid", "break", "test"), break_labels=NA)) # Non-numeric breaks
  expect_error(draw_discrete(x=rnorm(5), type="ordered",
                             breaks=c(1, 3, 2), break_labels=NA)) # Breaks out of order
  expect_error(draw_discrete(x=rnorm(5), type="ordered",
                             breaks=matrix(rep(c(0, 1, 2), 3), byrow=TRUE, ncol=3, nrow=3))) # Non-vector breaks
  expect_error(draw_discrete(x=rnorm(5), type="ordered",
                             breaks=c(-Inf, 0, Inf), break_labels=c(1))) # Invalid length break labels.
})

test_that("Ordered data valid tests", {
  skip("Rewriting draw functions")
  draw_discrete(rnorm(5),
                type = "ordered",
                breaks = c(-Inf, -1, 0, 1, Inf),
                break_labels = c("A", "B", "C", "D"))

  # Probit link
  draw_discrete(rnorm(5),
                type = "ordered",
                breaks = c(-Inf, 0, Inf),
                break_labels = c("A", "B"),
                link="probit")
})

test_that("Binary ICCs", {
  clusters = rep(1:5, 10)
  # Single probability
  draw_binary_icc(clusters = clusters)
  # Probability = length(cluster ids)
  draw_binary_icc(x = c(0.3, 0.5, 0.7, 0.8, 0.9), clusters = clusters)

  # Invalid cluster IDs
  expect_error(draw_binary_icc(clusters = data.frame(X=1:10, Y=1:10)))
  # X doesn't match cluster IDs
  expect_error(draw_binary_icc(x = c(0.5, 0.8), clusters = clusters))
  # X isn't a vector
  expect_error(draw_binary_icc(x = data.frame(j = c(0.1, 0.2),
                                              k = c(0.2, 0.4),
                                              m = c(0.3, 0.6),
                                              o = c(0.4, 0.8),
                                              p = c(0.5, 1.0)),
                               clusters = clusters))
  # X isn't numeric
  expect_error(draw_binary_icc(x = "hello", clusters = clusters))
  # X isn't a probability
  expect_error(draw_binary_icc(x = -0.5, clusters = clusters))
  # rho isn't a single number
  expect_error(draw_binary_icc(clusters = clusters, rho = c(0.5, 0.8)))
  # rho isn't a probability
  expect_error(draw_binary_icc(clusters = clusters, rho = 2))
  # rho isn't a number
  expect_error(draw_binary_icc(clusters = clusters, rho = "hello"))
  # Non-numeric N
  expect_error(draw_binary_icc(clusters = clusters, N = "hello"))
  # N provided but doesn't match
  expect_error(draw_binary_icc(clusters = clusters, N = 20))
})

test_that("Likert data example", {
  skip("Rewriting draw functions")
  set.seed(19861108)
  latent = rnorm(n=100, mean=3, sd=10)
  cutpoints = c(-15, -7, -3, 3, 7, 15)
  likert = draw_discrete(x=latent,
                         type="ordered",
                         breaks = cutpoints)
  expect_equal(length(unique(likert)), 7)
  expect_equal(max(likert), 7)
  expect_equal(min(likert), 1)

  draw_discrete(x=latent,
                type="ordered",
                breaks = cutpoints,
                break_labels = c("Strongly Disagree",
                                 "Disagree",
                                 "Lean Disagree",
                                 "No Opinion",
                                 "Lean Agree",
                                 "Agree",
                                 "Strongly Agree"))
})

test_that("Normal ICC", {
  clusters = rep(1:5, 10)
  # Single mean
  draw_normal_icc(clusters = clusters)
  # Means = length(cluster ids)
  draw_normal_icc(x = c(-1, -0.5, 0, 0.5, 1), clusters = clusters)

  # Invalid cluster IDs
  expect_error(draw_normal_icc(clusters = data.frame(X=1:10, Y=1:10)))
  # X doesn't match cluster IDs
  expect_error(draw_normal_icc(x = c(0.5, 0.8), clusters = clusters))
  # X isn't a vector
  expect_error(draw_normal_icc(x = data.frame(j = c(0.1, 0.2),
                                              k = c(0.2, 0.4),
                                              m = c(0.3, 0.6),
                                              o = c(0.4, 0.8),
                                              p = c(0.5, 1.0)),
                               clusters = clusters))
  # X isn't numeric
  expect_error(draw_normal_icc(x = "hello", clusters = clusters))
  # rho isn't a single number
  expect_error(draw_normal_icc(clusters = clusters, rho = c(0.5, 0.8)))
  # rho isn't a probability
  expect_error(draw_normal_icc(clusters = clusters, rho = 2))
  # rho isn't a number
  expect_error(draw_normal_icc(clusters = clusters, rho = "hello"))
  # Non-numeric N
  expect_error(draw_normal_icc(clusters = clusters, N = "hello"))
  # N provided but doesn't match
  expect_error(draw_normal_icc(clusters = clusters, N = 20))
  # SD is wrong length
  expect_error(draw_normal_icc(clusters = clusters, sd = c(1, 2)))
  # SD is non-numeric
  expect_error(draw_normal_icc(clusters = clusters, sd = "hello"))
  # SD is not a vector
  expect_error(draw_normal_icc(sd = data.frame(j = c(0.1, 0.2),
                                              k = c(0.2, 0.4),
                                              m = c(0.3, 0.6),
                                              o = c(0.4, 0.8),
                                              p = c(0.5, 1.0)),
                               clusters = clusters))
})

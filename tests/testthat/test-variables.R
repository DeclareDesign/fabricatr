context("Variable functions")

test_that("Variable functions", {
  # Single-level data, logit link, inherit or implicit N
  fabricate(my_level = level(
    N = 10,
    Y1 = rnorm(N),
    Y2 = draw_binary(Y1, link = "logit")
  ))

  # Single level, count, inherit or implicit N
  fabricate(my_level = level(
    N = 10,
    Y1 = rnorm(N, 5),
    Y2 = draw_discrete(Y1, type = "count", k = 3)
  ))

  # Draw binary, implicit N
  draw_binary(runif(100))
  # Draw count, implicit N
  draw_discrete(runif(100), type = "count", k = 4)

  # Logit link
  draw_binary(rnorm(5), link = "logit")
  # Probit link
  draw_binary(rnorm(5), link = "probit")
  # Identity link
  draw_binary(runif(5, 0, 1), link = "identity")

  # Count data
  draw_discrete(runif(5, 0, 5), type = "count", k = 5)
  draw_discrete(runif(5, 0, 1), type = "count", k = 5)

  # Verify we're not generating exactly the same data every time
  expect_equal(
    all(
      draw_discrete(runif(50, 0, 5), type="count", k = 5),
      draw_discrete(runif(50, 0, 5), type="count", k = 5)
    ), FALSE)

  # Binomial data, same k
  draw_discrete(x=c(0.2, 0.8), k=10, type="binomial")
  # Binomial data, different k for each obs.
  draw_discrete(x=c(0.2, 0.8), k=c(10, 100), type="binomial")
  # Binomial data, no k specified (should default to 1)
  draw_discrete(x=c(0.2, 0.8), type="binomial")

  # Binary data, invalid probabilities.
  expect_error(draw_binary(x=-1, N=10)) # Negative
  expect_error(draw_binary(x=c("invalid", "probability"), N=10)) # Non-numeric
  expect_error(draw_binary(x=1.2, N=10)) # Positive outside 0-1
  expect_error(draw_binary(x=c(0.5, 0.5, "invalid mixed"), N=10)) # Mixed non-numeric
  expect_error(draw_binary()) # No arguments
  expect_error(draw_binary(N=10)) # Missing probability
  expect_error(draw_binary(x=c(0.3, 0.4, 0.5), N=10)) # Not a multiple, don't want to recycle

  # Valid binary data
  draw_binary(x=c(0.5, 0.9), N=10)


  # Binomial data, invalid probabilities
  expect_error(draw_discrete(x=-1, N=10, type="binomial")) # Negative
  expect_error(draw_discrete(x=c("invalid", "probability"), N=10, type="binomial")) # Non-numeric
  expect_error(draw_discrete(x=1.2, N=10, type="binomial")) # Positive outside 0-1
  expect_error(draw_discrete(x=c(0.5, 0.5, "invalid mixed"), N=10, type="binomial")) # Mixed non-numeric
  expect_error(draw_discrete()) # No arguments
  expect_error(draw_discrete(N=10, type="binomial")) # Missing probability
  expect_error(draw_discrete(x=c(0.3, 0.4, 0.5), N=10, type="binomial")) # Not a multiple, don't want to recycle

  # Binomial data, invalid k
  expect_error(draw_discrete(x=c(0.2, 0.8), k=NA, type="binomial")) # NA
  expect_error(draw_discrete(x=c(0.2, 0.8), k="invalid", type="binomial")) # Character
  expect_error(draw_discrete(x=c(0.2, 0.8), k=0.5, type="binomial")) # Non-integer
  expect_error(draw_discrete(x=c(0.2, 0.8), k=-1, type="binomial")) # Negative integer
  expect_error(draw_discrete(x=c(0.2, 0.8), k=c(10, 100, 1000), type="binomial")) # Non-multiple
  expect_error(draw_discrete(x=c(0.2, 0.8), k=c(10, "mixed invalid"), type="binomial")) # Mixed non-integer

  # Valid binomial draw
  draw_discrete(x=0.5, k=10, N=25, type="binomial")

  # Invalid variable types
  expect_error(draw_discrete(x=0.5, N=5, type="invalid-type")) # Invalid variable type

  # Invalid link functions
  expect_error(draw_binary(rnorm(5), link = "link-that-doesn't-exist"))
  expect_error(draw_count(rnorm(5), k = 5, link = "link-that-doesn't-exist"))

  # Invalid count draws
  expect_error(draw_discrete(x=1, N=5, type="count", link="logit")) # Links are not allowed
  expect_error(draw_discrete(x="invalid", N=5, type="count")) # Invalid lambda
  expect_error(draw_discrete(x=-1, N=5, type="count")) # Invalid lambda, negative
  expect_error(draw_discrete(x=c(1, 2, 3, 4, -1), N=5, type="count")) # Mixed lambdas, one negative
  expect_error(draw_discrete(x=c(1, 2, 3, 4, "invalid"), N=5, type="count")) # Mixed lambdas, one character

  # Valid count draw
  draw_discrete(x=5, N=25, type="count")

  # Invalid categorical draws
  expect_error(draw_discrete(x=c(-1, 0, -0.5), N=3, type="categorical")) # Negative probability
  expect_error(draw_discrete(x="invalid", N=3, type="categorical")) # Non-numeric probability
  expect_error(draw_discrete(x=c(0.5, 0.75), N=10, type="categorical", link="probit")) # Link functions not accepted

  # Valid categorical draw
  draw_discrete(x=matrix(rep(c(0.3, 0.3, 0.4), 3), byrow=TRUE, ncol=3, nrow=3), N=3, type="categorical")

  # Convert vector of probabilities to matrix of probabilities
  expect_warning(draw_discrete(x=c(0.3, 0.3, 0.4), N=3, type="categorical"))

})

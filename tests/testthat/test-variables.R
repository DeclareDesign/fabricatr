context("Variable functions")

test_that("Variable functions", {
  fabricate(my_level = level(
    N = 10,
    Y1 = rnorm(N),
    Y2 = draw_binary(latent = Y1)
  ))

  fabricate(my_level = level(
    N = 10,
    Y1 = rnorm(N),
    Y2 = draw_count(latent = Y1, k = 3)
  ))

  draw_binary(latent = runif(100))
  draw_count(latent = runif(100), k = 4)


  draw_binary(latent = rnorm(5), link = "logit")
  draw_binary(latent = rnorm(5), link = "probit")
  draw_binary(prob = runif(5, 0, 1), link = "identity")

  draw_count(latent = rnorm(5), k = 5, link = "logit")
  draw_count(latent = rnorm(5), k = 5, link = "probit")
  draw_count(prob = runif(5, 0, 1), k = 5, link = "identity")

  expect_error(draw_binary(latent = data.frame(my_variable = runif(5, 0, 1))))
  expect_error(draw_count(latent = data.frame(my_variable = runif(5, 0, 1)), k = 5))

  expect_error(draw_binary(latent = rnorm(5), link = "link-that-doesn't-exist"))
  expect_error(draw_count(latent = rnorm(5), k = 5, link = "link-that-doesn't-exist"))

  # check for error if you send vectors that aren't probabilities to link identity
  expect_error(draw_count(prob = runif(5, 2, 3), k = 5, link = "identity"))
  expect_error(draw_binary(prob = runif(5, 2, 3), link = "identity"))

})


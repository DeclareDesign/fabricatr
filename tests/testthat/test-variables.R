context("Variable functions")

test_that("Variable functions", {
  fabricate_data(my_level = level(
    N = 10,
    Y1 = rnorm(N),
    Y2 = draw_binary(Y1)
  ))

  fabricate_data(my_level = level(
    N = 10,
    Y1 = rnorm(N),
    Y2 = draw_count(Y1, k = 3)
  ))

  draw_binary(runif(100))
  draw_count(runif(100), 4)


  draw_binary(rnorm(5), link = "logit")
  draw_binary(rnorm(5), link = "probit")
  draw_binary(runif(5, 0, 1), link = "identity")

  draw_count(rnorm(5), k = 5, link = "logit")
  draw_count(rnorm(5), k = 5, link = "probit")
  draw_count(runif(5, 0, 1), k = 5, link = "identity")

  expect_error(draw_binary(data.frame(my_variable = runif(5, 0, 1))))

  expect_error(draw_binary(rnorm(5), link = "link-that-doesn't-exist"))
  expect_error(draw_count(rnorm(5), link = "link-that-doesn't-exist"))

  # check for error if you send vectors that aren't probabilities to link identity
  expect_error(draw_count(runif(5, 2, 3), link = "identity"))
  expect_error(draw_binary(runif(5, 2, 3), link = "identity"))

})

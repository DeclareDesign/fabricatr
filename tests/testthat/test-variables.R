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

})


test_that("draw_multivariate", {

  skip_if_not_installed("MASS")

  set.seed(1)
  expect_equal(
    fabricate(N = 3,
              draw_multivariate(
                Y ~ MASS::mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(10, 3, 3, 2), 2, 2))
              )),
    structure(
      list(
        ID = c("1", "2", "3"),
        Y_1 = c(2.47556298325899,-0.473620723488142, 2.36978916324233),
        Y_2 = c(-0.856385953012281,-0.505205263050572, 1.65477933507106)
      ),
      class = "data.frame",
      row.names = c(NA,
                    3L)
    )
  )


  expect_equal(
    fabricate(N = 3,
              draw_multivariate(
                c(Y_1, Y_2) ~ MASS::mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(10, 3, 3, 2), 2, 2))
              )),
    structure(
      list(
        ID = c("1", "2", "3"),
        Y_1 = c(-1.6302319965645,-1.84501726908089,-1.6883744729295),
        Y_2 = c(-0.221503040728369,-2.20856302837875,-0.973722343467035)
      ),
      class = "data.frame",
      row.names = c(NA,
                    3L)
    )
  )

})


test_that("error", {

  skip_if_not_installed("MASS")

  expect_error(draw_multivariate(~ MASS::mvrnorm(
    n = 5,
    mu = c(0, 0),
    Sigma = matrix(c(10, 3, 3, 2), 2, 2)
  )),
  "Please provide a way")

  expect_error(draw_multivariate(c("nm1", "nm2", "nm3") ~ MASS::mvrnorm(
    n = 5,
    mu = c(0, 0),
    Sigma = matrix(c(10, 3, 3, 2), 2, 2)
  )),
  "You provided a different number of variable names")

})

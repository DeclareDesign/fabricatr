test_that("potential_outcomes", {
  set.seed(1)
  expect_equal(
    fabricate(
      N = 3,
      U = rnorm(N),
      potential_outcomes(Y ~ Z + U)
    ),
    structure(list(ID = c("1", "2", "3"), U = c(-0.626453810742332,
                                                0.183643324222082, -0.835628612410047), Y_Z_0 = c(-0.626453810742332,
                                                                                                  0.183643324222082, -0.835628612410047), Y_Z_1 = c(0.373546189257668,
                                                                                                                                                    1.18364332422208, 0.164371387589953)), class = "data.frame", row.names = c(NA,
                                                                                                                                                                                                                               3L))
  )

  expect_equal(
    fabricate(
      N = 3,
      potential_outcomes(Y ~ Z)
    ),

  )

})


test_that("draw_multivariate", {

  set.seed(1)
  expect_equal(
    fabricate(
      N = 3,
      draw_multivariate(Y ~ MASS:::mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(10,3,3,2),2,2)))
    ),
    structure(list(ID = c("1", "2", "3"), Y_1 = c(2.47556298325899,
                                                  -0.473620723488142, 2.36978916324233), Y_2 = c(-0.856385953012281,
                                                                                                 -0.505205263050572, 1.65477933507106)), class = "data.frame", row.names = c(NA,
                                                                                                                                                                             3L))
  )


  expect_equal(
    fabricate(
      N = 3,
      draw_multivariate(c(Y_1, Y_2) ~ MASS:::mvrnorm(N, mu = c(0, 0), Sigma = matrix(c(10,3,3,2),2,2)))
    ),
    structure(list(ID = c("1", "2", "3"), Y_1 = c(-1.6302319965645,
                                                  -1.84501726908089, -1.6883744729295), Y_2 = c(-0.221503040728369,
                                                                                                -2.20856302837875, -0.973722343467035)), class = "data.frame", row.names = c(NA,
                                                                                                                                                                             3L))
  )

})

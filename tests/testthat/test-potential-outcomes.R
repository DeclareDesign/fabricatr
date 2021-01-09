


test_that("potential_outcomes", {
  set.seed(1)
  expect_equal(
    fabricate(N = 3,
              U = rnorm(N),
              potential_outcomes(Y ~ Z + U)),
    structure(
      list(
        ID = c("1", "2", "3"),
        U = c(-0.626453810742332,
              0.183643324222082, -0.835628612410047),
        Y_Z_0 = c(-0.626453810742332,
                  0.183643324222082, -0.835628612410047),
        Y_Z_1 = c(0.373546189257668,
                  1.18364332422208, 0.164371387589953)
      ),
      class = "data.frame",
      row.names = c(NA,
                    3L)
    )
  )

  expect_equal(
    fabricate(N = 3,
              potential_outcomes(Y ~ Z)),
    structure(
      list(
        ID = c("1", "2", "3"),
        Y_Z_0 = c(0, 0, 0),
        Y_Z_1 = c(1,
                  1, 1)
      ),
      class = "data.frame",
      row.names = c(NA, 3L)
    )
  )

  expect_error(
    fabricate(
      N = 10,
      U = rnorm(N),
      potential_outcomes( ~ 0.1 * Z + U)),
    "Please provide an outcome name"
  )

})

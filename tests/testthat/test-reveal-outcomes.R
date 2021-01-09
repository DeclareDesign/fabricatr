
test_that("reveal outcomes", {

  # default conditions
  dat <- fabricate(
    N = 10,
    U = rnorm(N),
    potential_outcomes(Y ~ 0.1 * Z + U)
  )

  expect_equal(names(dat), c("ID", "U", "Y_Z_0", "Y_Z_1"))

  dat2 <- fabricate(
    data = dat,
    Z = rbinom(N, 1, prob = 0.5),
    Y = reveal_outcomes(Y ~ Z)
  )

  expect_equal(names(dat2), c("ID", "U", "Y_Z_0", "Y_Z_1", "Z", "Y"))


  # three conditions
  dat <- fabricate(
    N = 10,
    U = rnorm(N),
    potential_outcomes(Y ~ 0.1 * Z + U, conditions = list(Z = 1:3))
  )

  expect_equal(names(dat), c("ID", "U", "Y_Z_1", "Y_Z_2", "Y_Z_3"))

  dat2 <- fabricate(
    data = dat,
    Z = sample(1:3, N, replace = TRUE),
    Y = reveal_outcomes(Y ~ Z)
  )

  expect_equal(names(dat2), c("ID", "U", "Y_Z_1", "Y_Z_2", "Y_Z_3", "Z", "Y"))


  # two assignment variables
  dat <- fabricate(
    N = 10,
    U = rnorm(N),
    potential_outcomes(Y ~ 0.1 * Z + U, conditions = list(Z = 0:1, D = 0:1))
  )

  expect_equal(names(dat), c("ID", "U", "Y_Z_0_D_0", "Y_Z_1_D_0", "Y_Z_0_D_1", "Y_Z_1_D_1"))

  dat2 <- fabricate(
    data = dat,
    Z = sample(0:1, N, replace = TRUE),
    D = sample(0:1, N, replace = TRUE),
    Y = reveal_outcomes(Y ~ Z + D)
  )

  expect_equal(names(dat2), c("ID", "U", "Y_Z_0_D_0", "Y_Z_1_D_0", "Y_Z_0_D_1", "Y_Z_1_D_1", "Z", "D", "Y"))

})

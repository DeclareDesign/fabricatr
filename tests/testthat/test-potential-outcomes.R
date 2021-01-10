






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

  expect_error(fabricate(
    N = 10,
    U = rnorm(N),
    potential_outcomes( ~ 0.1 * Z + U)
  ),
  "Please provide an outcome name")

  dat <- fabricate(N = 3, U = 1:3)
  expect_equal(
    fabricate(data = dat,
              potential_outcomes(Y ~ Z)),
    structure(
      list(
        ID = c("1", "2", "3"),
        U = 1:3,
        Y_Z_0 = c(0, 0,
                  0),
        Y_Z_1 = c(1, 1, 1)
      ),
      class = "data.frame",
      row.names = c(NA,
                    3L)
    )
  )

  expect_equal(
    fabricate(
      level1 = add_level(N = 2),
      level2 = add_level(N = 2, potential_outcomes(Y ~ 0.1))
    ),
    structure(
      list(
        level1 = c("1", "1", "2", "2"),
        level2 = c("1",
                   "2", "3", "4"),
        Y_Z_0 = c(0.1, 0.1, 0.1, 0.1),
        Y_Z_1 = c(0.1,
                  0.1, 0.1, 0.1)
      ),
      class = "data.frame",
      row.names = c(NA, 4L)
    )

  )

  expect_equal(
    fabricate(
      year = add_level(N = 2, year_shock = c(0, 1)),
      country = add_level(
        N = 2,
        country_shock = rep(0.5, N),
        nest = FALSE
      ),
      obs = cross_levels(
        by = join(year, country),
        GDP_it = country_shock + year_shock,
        potential_outcomes(Y ~ GDP_it)
      )
    ),
    structure(
      list(
        year = c("1", "2", "1", "2"),
        year_shock = c(0,
                       1, 0, 1),
        country = c("1", "1", "2", "2"),
        country_shock = c(0.5,
                          0.5, 0.5, 0.5),
        obs = c("1", "2", "3", "4"),
        GDP_it = c(0.5,
                   1.5, 0.5, 1.5),
        Y_Z_0 = c(0.5, 1.5, 0.5, 1.5),
        Y_Z_1 = c(0.5,
                  1.5, 0.5, 1.5)
      ),
      class = "data.frame",
      row.names = c(NA, 4L)
    )
  )

  expect_equal(
    fabricate(
      year = add_level(
        N = 2,
        year_shock = c(0, 1),
        potential_outcomes(Y ~ year_shock)
      ),
      country = add_level(
        N = 2,
        country_shock = rep(0.5, N),
        nest = FALSE
      ),
      obs = cross_levels(by = join(year, country),
                         GDP_it = country_shock + year_shock)
    ),
    structure(
      list(
        year = c("1", "2", "1", "2"),
        year_shock = c(0,
                       1, 0, 1),
        Y_Z_0 = c(0, 1, 0, 1),
        Y_Z_1 = c(0, 1, 0, 1),
        country = c("1",
                    "1", "2", "2"),
        country_shock = c(0.5, 0.5, 0.5, 0.5),
        obs = c("1",
                "2", "3", "4"),
        GDP_it = c(0.5, 1.5, 0.5, 1.5)
      ),
      class = "data.frame",
      row.names = c(NA,
                    4L)
    )
  )

  expect_equal(
    fabricate(
      year = add_level(N = 2, year_shock = c(0, 1)),
      country = add_level(
        N = 2,
        country_shock = rep(0.5, N),
        potential_outcomes(Y ~ country_shock),
        nest = FALSE
      ),
      obs = cross_levels(by = join(year, country),
                         GDP_it = country_shock + year_shock)
    ),
    structure(
      list(
        year = c("1", "2", "1", "2"),
        year_shock = c(0,
                       1, 0, 1),
        country = c("1", "1", "2", "2"),
        country_shock = c(0.5,
                          0.5, 0.5, 0.5),
        Y_Z_0 = c(0.5, 0.5, 0.5, 0.5),
        Y_Z_1 = c(0.5,
                  0.5, 0.5, 0.5),
        obs = c("1", "2", "3", "4"),
        GDP_it = c(0.5,
                   1.5, 0.5, 1.5)
      ),
      class = "data.frame",
      row.names = c(NA, 4L)
    )
  )

})

context("Fabricate")

test_that("Fabricate", {
  fabricate(N = 2)
  fabricate(N = 2, Y = 10)
  fabricate(N = 2, Y = 10, ID_label = "test")
  fabricate(N = 2, Y1 = 5, Y2 = rnorm(N))

  expect_equal(max(fabricate(
    N = 2, A = 1:2, A = 3
  )$A),  3)

  df <- fabricate(N = 2, Y = 10)
  df
  fabricate(data = df, Y2 = Y + 1)

  fabricate(
    data = data.frame(existing_data = rnorm(5)),
    Y1 = rnorm(N),
    Y2 = rnorm(N)
  )
  fabricate(
    data = data.frame(existing_data = rnorm(5)),
    Y1 = rnorm(N),
    Y2 = rnorm(N),
    ID_label = "ID"
  )

  fabricate(regions = add_level(N = 5, gdp = rnorm(N)))

  fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = sample(1:5), subways = gdp + 10)
  )

  fabricate(regions = add_level(N = 5),
                 cities = add_level(N = sample(1:5), subways = rnorm(N, mean = 5)))

  fabricate(
    regions = add_level(N = 5, gdp = runif(N)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = 5))
  )

  # User provides matrix, test conversion.
  fabricate(data = matrix(rep(c(1, 2, 3), 3), byrow=TRUE, ncol=3, nrow=3))
})

test_that("choose N of a level based on data from higher levels", {
  fabricate(
    regions = add_level(N = 2, gdp = runif(N)),
    cities = add_level(
      N = round(gdp) * 10 + 1,
      subways = rnorm(N, mean = 5)
    )
  )
})


test_that("trigger errors", {
  expect_error(fabricate(
    regions = add_level(),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = 5))
  ))

  expect_error(fabricate(
    regions = add_level(N = c(1, 2)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = 5))
  ))

  expect_error(fabricate(
    regions = add_level(N = 2),
    cities = add_level(N = c(5, 5, 5), subways = rnorm(N, mean = 5))
  ))

  expect_error(fabricate(
    regions = add_level(N = 2),
    cities = add_level(N = "N that is a character vector", subways = rnorm(N, mean = 5))
  ))

  expect_error(fabricate(
    regions = add_level(N = rep(5, 2)),
    cities = add_level(N = c(5, 5, 5), subways = rnorm(N, mean = 5))
  ))

  # you must provide name for levels
  expect_error(fabricate(level(N = 5,
                                    gdp = rnorm(N)),
                              add_level(
                                N = sample(1:5),
                                subways = rnorm(N, mean = gdp)
                              )))

  # No N, no data
  expect_error(fabricate(test1 = runif(10), test2 = test1 * 3 * runif(10, 1, 2)))

  # Non-integer N:
  expect_error(fabricate(N = 3.5, test1=runif(3)))

  # Vector N:
  expect_error(fabricate(N = c(3, 4), test1=runif(3)))
  expect_error(fabricate(N = c(3, 4), test1=runif(3), ID_label="my_id"))

  # Non-numeric N
  expect_error(fabricate(N = "hello", test1=runif(3)))

  # Negative N
  expect_error(fabricate(N = -1, test1=runif(10)))

  # Sending a scalar will coerce to a data.frame
  fabricate(data = c(5))

  # Vector as ID_label
  expect_error(fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label=c("invalid", "id")))
  # Matrix as ID_label
  expect_error(fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label=matrix(rep(c(1,2,3),3), byrow=TRUE, ncol=3, nrow=3)))
  # Numeric as ID_label
  expect_warning(fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label=7))
  # Character as ID_label
  fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label="hello")
  fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label=c("hello"))
  # Symbol as ID_label
  expect_error(fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label=test1))
  expect_error(fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label=test3))

  # Unusual test with implicit data argument
  expect_error(fabricate(N=10, 1:N))
})

test_that("unusual pass of add_level call to single level generation as data matrix", {
  expect_error(fabricate(add_level(N = 5,
                                   gdp = rnorm(N))))
})

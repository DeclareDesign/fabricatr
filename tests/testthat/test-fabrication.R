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

  fabricate(regions = level(N = 5, gdp = rnorm(N)))

  fabricate(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = sample(1:5), subways = gdp + 10)
  )

  fabricate(regions = level(N = 5),
                 cities = level(N = sample(1:5), subways = rnorm(N, mean = 5)))

  fabricate(
    regions = level(N = 5, gdp = runif(N)),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = 5))
  )

  # User provides matrix, test conversion.
  fabricate(data = matrix(rep(c(1, 2, 3), 3), byrow=TRUE, ncol=3, nrow=3))
})

test_that("use a function to choose N of a level", {
  fabricate(
    regions = level(N = 2, gdp = runif(N)),
    cities = level(
      N = function(x)
        return(round(gdp) * 10 + 1),
      subways = rnorm(N, mean = 5)
    )
  )
})


test_that("trigger errors", {
  expect_error(fabricate(
    regions = level(),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = 5))
  ))

  expect_error(fabricate(
    regions = level(N = c(1, 2)),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = 5))
  ))

  expect_error(fabricate(
    regions = level(N = 2),
    cities = level(N = c(5, 5, 5), subways = rnorm(N, mean = 5))
  ))

  expect_error(fabricate(
    regions = level(N = 2),
    cities = level(N = "N that is a character vector", subways = rnorm(N, mean = 5))
  ))

  region_data <- data.frame(capital = c(1, 0, 0, 0, 0))
  expect_error(fabricatr:::fabricate_data_single_level(data = region_data, N = 5, gdp = runif(N)))

  expect_error(fabricate(
    regions = level(N = rep(5, 2)),
    cities = level(N = c(5, 5, 5), subways = rnorm(N, mean = 5))
  ))

  expect_error(fabricatr:::fabricate_data_single_level(
    N = c(5, 2),
    gdp = runif(N),
    ID_label = "my-level"
  ))

  # you must provide name for levels
  expect_error(fabricate(level(N = 5,
                                    gdp = rnorm(N)),
                              level(
                                N = sample(1:5),
                                subways = rnorm(N, mean = gdp)
                              )))

  # same for a single level
  expect_error(fabricate(level(N = 5,
                                    gdp = rnorm(N))))

  # must send a data frame to data
  expect_error(user_data <-
                 fabricate(data = c(5)))

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
  fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label=test1)
  fabricate(N=10, test1=rnorm(10), test2=rpois(10, lambda=2), ID_label=test3)
})

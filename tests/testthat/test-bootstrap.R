context("Bootstrap")

test_that("Bootstrap", {
  two_levels <- fabricate(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = 5, subways = rnorm(N, mean = gdp))
  )

  # Example with data.table codepath
  resampled_two_levels <- resample_data(two_levels, N = c(2, 2),
                                        ID_labels = c("regions", "cities"))

  # Example without data.table codepath
  resampled_two_levels <- .resample_data_internal(two_levels, N = c(2, 2),
                                                  ID_labels = c("regions", "cities"),
                                                  use_dt=0)

  expect_equal(nrow(resampled_two_levels), 4)

  resampled_two_levels <- resample_data(two_levels, 5)

  expect_equal(nrow(resampled_two_levels), 5)
})

test_that("Error handling of Bootstrap", {
  two_levels <- fabricate(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  resampled_two_levels <- resample_data(two_levels) # Missing N

  # Invalid ID
  expect_error(resample_data(two_levels, c(100, 10), ID_labels = c("Invalid_ID", "Invalid_ID_2")))
  # ID length doesn't match n length
  expect_error(resample_data(two_levels, c(100, 10), ID_labels = c("regions")))
  # Negative N
  expect_error(resample_data(two_levels, c(-1), ID_labels = c("regions")))
  # Non-numeric
  expect_error(resample_data(two_levels, c("hello world"), ID_labels = c("regions")))
})

test_that("Direct bootstrap_single_level", {
  two_levels <- fabricate(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  null_data = two_levels[two_levels$gdp > 100, ]
  # Trying to bootstrap null data
  expect_equal(dim(null_data)[1], 0)
  expect_error(bootstrap_single_level(null_data, ID_label="regions", N=10))

  # Trying to bootstrap single level with an invalid ID.
  expect_error(bootstrap_single_level(two_levels, ID_label="invalid-id", N=10))
})

test_that("Extremely high volume data creation.", {
  skip("Slows build substantially.")
  deep_dive_data = fabricate(
    countries = level(N = 100, gdp = rlnorm(N)),
    states = level(N = 50, population = rlnorm(N)),
    cities = level(N = 50, holiday = runif(N, 1, 365)),
    neighborhoods = level(N = 5, stoplights = draw_binary(x=0.5, N)),
    houses = level(N = 5, population = runif(N, 1, 5)),
    people = level(N = population, sex = ifelse(draw_binary(x=0.5, N), "M", "F"))
  )

  test_resample = resample_data(deep_dive_data,
                                ID_labels=c("countries", "states", "cities"),
                                N=c(100, 50, 50))
})

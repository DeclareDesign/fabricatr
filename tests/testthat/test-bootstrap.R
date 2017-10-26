context("Bootstrap")

test_that("Bootstrap", {
  two_levels <- fabricate(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = 5, subways = rnorm(N, mean = gdp))
  )

  resampled_two_levels <- resample_data(two_levels, N = c(2, 2), ID_labels = c("regions", "cities"))

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
  expect_error(resample_data(two_levels, c(100, 10), ID_labels = c("Invalid_ID")))
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

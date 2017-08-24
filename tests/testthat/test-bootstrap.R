context("Bootstrap")

test_that("Bootstrap", {
  two_levels <- fabricate_data(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = 5, subways = rnorm(N, mean = gdp))
  )

  resampled_two_levels <- resample_data(two_levels, N = c(2, 2), ID_labels = c("regions", "cities"))

  expect_equal(nrow(resampled_two_levels), 4)

  resampled_two_levels <- resample_data(two_levels, 5)

  expect_equal(nrow(resampled_two_levels), 5)
})

test_that("Error handling of Bootstrap", {
  two_levels <- fabricate_data(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  resampled_two_levels <- resample_data(two_levels) # Missing N
  expect_error(resample_data(two_levels, c(100, 10), ID_labels = c("Invalid_ID")))
})

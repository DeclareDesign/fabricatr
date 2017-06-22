context("Bootstrap")

test_that("Bootstrap",{

two_levels <- fabricate_data(regions = level(N = 5, gdp = rnorm(N)),
               cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

head(two_levels)

q <- resample_data(two_levels, c(100, 10), ID_labels = c("regions_ID", "cities_ID"))

q <- resample_data(two_levels, 5)
})

test_that("Error handling of Bootstrap",{
  two_levels <- fabricate_data(regions = level(N = 5, gdp = rnorm(N)),
                               cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

  q <- resample_data(two_levels) # Missing N
  expect_error(resample_data(two_levels, c(100, 10), ID_labels=c("Invalid_ID")))
})

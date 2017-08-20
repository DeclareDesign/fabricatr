context("Start with existing multi-level data and add variables")

test_that("Start with existing multi-level data and add variables",{

  user_data <-
  fabricate_data(
    regions = level(N = 5, gdp = rnorm(N)))

  expect_equal(dim(user_data), c(5, 2))

  user_data <-
    fabricate_data(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

  expect_equal(dim(user_data), c(15, 4))

  ## add a variable at the region level
  user_data_2 <-
    fabricate_data(data = user_data,
                   regions = level(rob = paste0(regions, "r")))

  expect_equal(dim(user_data_2), c(15, 5))

  ## add a variable at the cities level
  user_data_3 <-
  fabricate_data(data = user_data,
                 cities = level(rob = paste0(cities, "c")))

  expect_equal(dim(user_data_3), c(15, 5))

  ## do both
  ## note this will break if you try to use cities_ID at the region level (intentional)!
  user_data_4 <-
  fabricate_data(data = user_data,
                 regions = level(rob = paste0(regions, "r")),
                 cities = level(bob = paste0(cities, "c")))

  expect_equal(dim(user_data_4), c(15, 6))

  ## do both and create a new level at the bottom level
  ## note this will break if you try to use cities_ID at the region level (intentional)!
  user_data_5 <-
  fabricate_data(data = user_data,
                 regions = level(rob = paste0(regions, "r")),
                 cities = level(bob = paste0(cities, "c")),
                 neighborhoods = level(N = 10, tmp = rnorm(N)))

  expect_equal(dim(user_data_5), c(150, 8))


})

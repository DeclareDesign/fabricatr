context("Start with existing multi-level data and add variables")

test_that("Start with existing multi-level data and add variables",{

  user_data <- fabricate_data(
    regions = level(N = 5, gdp = rnorm(N)),
    cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

  ## add a variable at the region level
  fabricate_data(data = user_data,
                 regions = level(rob = paste0(regions, "r") )) %>% head

  ## add a variable at the cities level
  fabricate_data(data = user_data,
                 cities = level(rob = paste0(cities, "c"))) %>% head

  ## do both
  ## note this will break if you try to use cities_ID at the region level (intentional)!
  fabricate_data(data = user_data,
                 regions = level(rob = paste0(regions, "r")),
                 cities = level(bob = paste0(cities, "c"))) %>% head

  ## do both and create a new level at the bottom level
  ## note this will break if you try to use cities_ID at the region level (intentional)!
  fabricate_data(data = user_data,
                 regions = level(rob = paste0(regions, "r")),
                 cities = level(bob = paste0(cities, "c")),
                 neighborhoods = level(N = 10, tmp = rnorm(N))) %>% head

})

context("Start with existing multi-level data and add variables")

test_that("Start with existing multi-level data and add variables",{

  countries_data <-
    fabricate_data(N = 2,
                   ID_label = countries,
                   gdp = rnorm(N))

  regions_data <- fabricate_data(countries = level(N = 2),
                                 regions = level(N = 2, elevation = rnorm(N)))


  full_data <- fabricate_data(
    countries = level(data = countries_data,
                      new_country_variable = rnorm(N)),
    regions = level(data = regions_data, by = "countries",
                    new_region_variable = rnorm(N))
  )

  full_data <- fabricate_data(
    countries = level(N = 2),
    regions = level(data = regions_data, by = "countries",
                    new_region_variable = rnorm(N))
  )

})

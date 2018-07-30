context("Start with existing multi-level data and add variables")

test_that("Import data plus single-level add variables.", {
  result <- fabricate(mtcars, zort = drat * 2)
  expect_equal(dim(result), c(32, 13))
})

test_that("Start with existing multi-level data and add variables", {
  user_data <-
    fabricate(
      regions = add_level(N = 5, gdp = rnorm(N)),
      cities = add_level(N = sample(1:5), subways = rnorm(N, mean = gdp))
    )
  expect_equal(dim(user_data), c(15, 4))

  ## add a variable at the region level

  user_data_2 <-
    fabricate(
      data = user_data,
      regions = modify_level(rob = paste0(regions, "r"))
    )
  expect_equal(dim(user_data_2), c(15, 5))

  ## add a variable at the cities level
  user_data_3 <-
    fabricate(
      data = user_data,
      cities = modify_level(rob = paste0(cities, "c"))
    )
  expect_equal(dim(user_data_3), c(15, 5))

  # Multiple modify calls
  user_data_4 <-
    fabricate(
      data = user_data,
      regions = modify_level(rob = paste0(regions, "r")),
      cities = modify_level(bob = paste0(cities, "c"))
    )
  expect_equal(dim(user_data_4), c(15, 6))

  # Modify then add call
  user_data_5 <-
    fabricate(
      data = user_data,
      regions = modify_level(rob = paste0(regions, "r")),
      cities = modify_level(bob = paste0(cities, "c")),
      neighborhoods = add_level(N = 10, tmp = rnorm(N))
    )
  expect_equal(dim(user_data_5), c(150, 8))
})

test_that("Modify variable at wrong level", {
  expect_error(
    df <- fabricate(
      country = add_level(N = 50, population = runif(N, 10000, 20000)),
      state = add_level(N = 10, latitude = runif(N, 40, 50)),
      town = add_level(N = 5, stop_lights = draw_binary(prob = 0.7, N = N)),

      state = modify_level(
        crime = 0.5 + stop_lights + latitude)
    )
  )
})

test_that("Import -> nest with special length N, test for #80", {
  df1 <- fabricate(N = 3, ID_label = "city")
  df2 <- fabricate(df1, neighborhood = add_level(N = c(10, 20, 30)))
  expect_equal(nrow(df2), 60)
})

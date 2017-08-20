context("hierarchical")

library(dplyr)
test_that("hierarchical data is created correctly when you have a vector variable that is of length N per level",{

  hierarchy <- fabricate_data(
    regions = level(N = 3, gdp = rnorm(N)),
    districts = level(
      N = 2,
      var1 = c("recent", "ancient"),
      var2 = ifelse(var1 == "recent", gdp, 5)
    ),
    cities = level(N = 2, subways = rnorm(N, mean = gdp))
  )


  expect_equal(
    hierarchy %>%
      distinct(regions, var2) %>%
      group_by(regions) %>%
      distinct(var2) %>%
      tally %>%
      .$n,
    rep(2, 3)
  )

  expect_equal(hierarchy$var2[hierarchy$var1 == "ancient"], rep(5, 6))

})

test_that("creating variables", {
  population <- fabricate_data(
    block = level(
      N = 5,
      block_effect = rnorm(N)
    ),
    individuals = level(N = 10, noise = rnorm(N))
  )

  expect_error(fabricate_data(
    N = 10^5,
    noise = rnorm(N),
    block = level(
      N = 5^4,
      block_effect = rnorm(N)
    )
  ))
})

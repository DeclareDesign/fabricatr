context("hierarchical")

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

df_2 <- unique(hierarchy[,c("regions", "var2")])

  expect_equivalent(
    sapply(split(df_2, df_2$regions), function(x) length(x$var2)),
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

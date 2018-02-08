context("hierarchical")

test_that("hierarchical data is created correctly when you have a vector variable that is of length N per level", {
  hierarchy <- fabricate(
    regions = add_level(N = 3, gdp = rnorm(N)),
    districts = add_level(
      N = 2,
      var1 = c("recent", "ancient"),
      var2 = ifelse(var1 == "recent", gdp, 5)
    ),
    cities = add_level(N = 2, subways = rnorm(N, mean = gdp))
  )

  df_2 <- unique(hierarchy[, c("regions", "var2")])

  expect_equivalent(
    vapply(split(df_2, df_2$regions),
           function(x) length(x$var2),
           numeric(1)),
    rep(2, 3)
  )

  expect_equal(hierarchy$var2[hierarchy$var1 == "ancient"], rep(5, 6))
})

test_that("Basic variable creation", {
  population <- fabricate(
    block = add_level(
      N = 5,
      block_effect = rnorm(N)
    ),
    individuals = add_level(N = 2, noise = rnorm(N))
  )
  expect_equal(nrow(population), 10)
  expect_equal(ncol(population), 4)
})

test_that("Mixing level calls and variable creation calls.", {
  expect_error(fabricate(
    N = 10,
    noise = rnorm(N),
    block = add_level(
      N = 5,
      block_effect = rnorm(N)
    )
  ))
})


test_that("Nested level where inner level N depends on outer level N", {
  # Test the implicit N
  nl_test <- fabricate(
    outer = add_level(N = 25),
    inner = add_level(N = sample(1:100, N))
  )

  # Verify the sample worked
  nr <- unlist(lapply(split(nl_test, nl_test$outer), function(x) { nrow(x) }))
  expect_true(var(nr) != 0)
  expect_lte(max(nr), 100)
  expect_gte(min(nr), 1)
})

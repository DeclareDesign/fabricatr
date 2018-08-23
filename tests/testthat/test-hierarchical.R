context("hierarchical")

test_that("Specifying N for each level.", {
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
    inner = add_level(N = sample(1:100, length(outer)))
  )

  # Verify the sample worked
  nr <- unlist(lapply(split(nl_test, nl_test$outer), function(x) { nrow(x) }))
  expect_true(var(nr) != 0)
  expect_lte(max(nr), 100)
  expect_gte(min(nr), 1)

  # And now try with N and have it fail:
  expect_error(fabricate(
    outer = add_level(N = 25),
    inner = add_level(N = sample(1:100, N))
  ))
})

test_that("modify is the same as fabricate when no lower-level variation", {
  # Test the implicit N
  data <- structure(list(villages = c("1", "1", "1", "1", "2", "2", "2",
                                      "2", "3", "3", "3", "3"),
                         elevation = c(-2.48889487515394, -2.48889487515394,
                                       -2.48889487515394, -2.48889487515394,
                                       -1.13351774554235, -1.13351774554235,
                                       -1.13351774554235, -1.13351774554235,
                                       -1.02483410795231, -1.02483410795231,
                                       -1.02483410795231, -1.02483410795231),
                         citizens = c("01", "02","03", "04", "05", "06", "07",
                                      "08", "09", "10", "11", "12"),
                         income = c(0.767469110433012, 0.156904492527246,
                                    0.0925139961764216,
                                    0.225728516001254, 0.665376711403951,
                                    0.30171422380954, 0.38407509541139,
                                    0.869735463289544, 0.560231429291889,
                                    0.00678953621536493,
                                    0.620438944082707, 0.369847694411874)),
                    .Names = c("villages","elevation", "citizens", "income"),
                    row.names = c(NA, -12L), class = "data.frame")

  # Verify the sample worked

  fab <- fabricate(data, Z = 0, Y_vil_Z_0 = elevation + 5 + 2 * Z, Z = 1,
                   Y_vil_Z_1 = elevation + 5 + 2 * Z, Z = NULL,
                   ID_label = "citizens")
  mod <- fabricate(data,
                   villages=modify_level(Z = 0,
                                         Y_vil_Z_0 = elevation + 5 + 2 * Z,
                                         Z = 1,
                                         Y_vil_Z_1 = elevation + 5 + 2 * Z,
                                         Z = NULL))

  expect_identical(fab, mod)
})

test_that("you can create structure without variables", {

  expect_equivalent(nrow(fabricate(
    primary_schools   = add_level(N = 5),
    secondary_schools = add_level(N = 6, nest = FALSE),
    students          =
      link_levels(N = 15, by = join(primary_schools, secondary_schools))
  )), 15)

})

test_that("Nested level edge case",{
  expect_error(fabricate(e=nest_level(N=2)), "top level")


})


test_that("modify_level hierarchical edge cases", {

  expect_equal(fabricate(a=add_level(N=10), b = add_level(2), a=modify_level(x=1))$x, rep(1,10))

})

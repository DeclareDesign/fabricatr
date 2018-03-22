context("Resampling")

test_that("Resampling", {
  two_levels <- fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = 5, subways = rnorm(N, mean = gdp))
  )
  expect_equal(nrow(two_levels), 25)
  expect_equal(all(table(two_levels$regions) == 5), TRUE)

  # Example with data.table codepath
  resampled_two_levels <- resample_data(
    two_levels, N = c(2, 2),
    ID_labels = c("regions", "cities")
  )
  expect_equal(dim(resampled_two_levels)[1], 4)

  # Example without data.table codepath
  resampled_two_levels <- fabricatr:::.resample_data_internal(
    two_levels, N = c(2, 2),
    ID_labels = c("regions", "cities"),
    use_dt = FALSE
  )
  expect_equal(dim(resampled_two_levels)[1], 4)

  resampled_two_levels_again <- resample_data(two_levels, 5)
  expect_equal(nrow(resampled_two_levels_again), 5)
})

test_that("Resampling: Bootstrap call, no N provided", {
  two_levels <- fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  resampled_two_levels <- resample_data(two_levels) # Missing N
  expect_equal(nrow(resampled_two_levels), nrow(two_levels))
})

test_that("Bootstrapping error handling.", {
  two_levels <- fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  # Invalid ID
  expect_error(resample_data(two_levels, c(100, 10),
                             ID_labels = c("Invalid_ID", "Invalid_ID_2")))
  # ID length doesn't match n length
  expect_error(resample_data(two_levels, c(100, 10), ID_labels = c("regions")))
  # Negative N
  expect_error(resample_data(two_levels, c(-1), ID_labels = c("regions")))
  # Non-numeric N
  expect_error(resample_data(two_levels, c("hello world"),
                             ID_labels = c("regions")))
  # Non-numeric N in direct call of resample_single_level. This is unlikely to
  # arise normally since we don't export it and the code paths that call it have
  # separate error handling
  expect_error(resample_single_level(two_levels, N = c(1, 2),
                                     ID_label = "regions"))
  expect_error(resample_single_level(two_levels, N = 1.5,
                                     ID_label = "regions"))
  expect_error(resample_single_level(two_levels, N = "hello",
                                     ID_label = "regions"))
})

test_that("Direct resample_single_level", {
  two_levels <- fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  null_data <- two_levels[two_levels$gdp > 100, ]
  # Trying to resample null data
  expect_equal(dim(null_data)[1], 0)
  expect_error(resample_single_level(null_data, ID_label = "regions", N = 10))

  # Trying to resample single level with an invalid ID.
  expect_error(resample_single_level(two_levels,
                                     ID_label = "invalid-id", N = 10))
})

test_that("Extremely deep resampling", {
  rect_data <- fabricate(
    N = 10,
    xA = 1:10,
    xB = 11:20,
    xC = 21:30,
    xD = 31:40,
    xE = 41:50,
    xF = 51:60,
    xG = 61:70,
    xH = 71:80,
    xI = 81:90,
    xJ = 91:100,
    xK = 101:110
  )

  expect_error(resample_data(
    rect_data,
    N = c(
      xA = 5,
      xB = 3,
      xC = 6,
      xD = 7,
      xE = 3,
      xF = 1,
      xG = 2,
      xH = ALL,
      xI = 2,
      xJ = 4,
      xK = 9
    )
  ))
})

test_that("Extremely high volume data creation.", {
  skip("Slows build substantially.")
  deep_dive_data <- fabricate(
    countries = add_level(N = 100, gdp = rlnorm(N)),
    states = add_level(N = 50, population = rlnorm(N)),
    cities = add_level(N = 50, holiday = runif(N, 1, 365)),
    neighborhoods = add_level(N = 5, stoplights = draw_binary(x = 0.5, N)),
    houses = add_level(N = 5, population = runif(N, 1, 5)),
    people = add_level(N = population,
                       sex = ifelse(draw_binary(x = 0.5, N), "M", "F"))
  )

  test_resample <- resample_data(
    deep_dive_data,
    ID_labels = c("countries", "states", "cities"),
    N = c(100, 50, 50)
  )
})

test_that("Multi-level Resample validity", {
  set.seed(19861108)

  two_levels <- fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  resample_validity <- resample_data(two_levels,
                                     N = c(regions = 6, cities = 5))
  # Region-level variables are still constant
  expect_true(
    all(lapply(
      split(resample_validity$gdp, resample_validity$regions),
      function(x) { length(unique(x)) }
    ) == TRUE)
  )

  # Ensure that even as regions are resampled, cities are uniquely sampled
  expect_equal(
    length(unique(split(resample_validity$cities, rep(1:6, each=5)))),
    6)
})

test_that("Resample errors", {
  two_levels <- fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  # Mixed specification of names
  expect_error(resample_data(
    two_levels,
    N = c(3, cities = 5),
    ID_labels = c("regions", "cities")
  ))

  # Invalid IDs
  expect_error(resample_data(
    two_levels,
    N = c(invalidid = 3, cities = 5)
  ))

  # Incomplete N specification
  expect_error(resample_data(
    two_levels,
    N = c(3, cities = 5)
  ))

  # No N specification
  expect_error(resample_data(
    two_levels,
    N = c(3, 5)
  ))
})

test_that("Passthrough resampling.", {
  two_levels <- fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = gdp))
  )

  passthrough <- resample_data(two_levels, N = c(regions = ALL, cities = 3))
  expect_equal(length(unique(passthrough$regions)), 5)
  expect_equal(nrow(passthrough), 15)

  # Warning when final level resampled has passthrough -- this is superfluous
  expect_warning(resample_data(two_levels, N = c(regions = ALL, cities = ALL)))
})

test_that("Unique labels", {
  df_test <- fabricate(L1 = add_level(N = 26, L1C = LETTERS),
                       L2 = add_level(N = 26, L2C = letters))

  sample_resample <- resample_data(df_test,
                                  N = c("L1C" = 30,
                                        "L2C" = 30),
                                  unique_labels = TRUE)
  expect_equal(length(unique(sample_resample$L1C_unique)), 30)
  expect_equal(length(unique(sample_resample$L2C_unique)), 900)
  expect_equal(nrow(sample_resample), 900)
  expect_equal(ncol(sample_resample), 6)

  sample_resample_upperonly <- resample_data(df_test,
                                            N = c("L1C" = 15),
                                            unique_labels = TRUE)
  expect_equal(ncol(sample_resample_upperonly), 5)
  expect_equal(nrow(sample_resample_upperonly), 15 * 26)
  expect_equal(length(unique(sample_resample_upperonly$L1C_unique)), 15)
})

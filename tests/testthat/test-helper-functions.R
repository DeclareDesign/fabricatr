context("Helper functions")

test_that("Error handlers: handle_data", {
  # User passed in data that isn't a data frame (no dimensionality)
  expect_error(handle_data(1:10))

  # It's not a data frame and the user didn't specify the argument name
  # this is a weird case involving arguments being interpreted by position
  # from fabricate and weird stuff flipping through to handle_data
  expect_error(handle_data(matrix(1:9, nrow = 3, ncol = 3)))

  # Test coercion of dimensional but non-df objects -- a matrix is most common
  # example -- this is also a working use case.
  handle_data(data = matrix(1:9, nrow = 3, ncol = 3))

  # Silly test -- object has dimensionality but won't coerce to df this should
  # almost never happen except for very poorly behaved objects
  X <- 1:10
  Y <- X * 2 + rnorm(10)
  df <- data.frame(X = X, Y = Y)
  model <- lm(Y ~ X, df)
  dim(model) <- c(3, 4) # This will break the model object, but fine for test
  expect_error(handle_data(data = model))
})

test_that("Error handlers: handle_id", {
  # Unlikely scenario where we're asked to generate an ID
  # but our 6 default variables are all taken
  data <- data.frame(
    ID = 1:10,
    fab_ID_1 = 11:20,
    fab_ID_2 = 21:30,
    fab_ID_3 = 31:40,
    fab_ID_4 = 41:50,
    fab_ID_5 = 51:60
  )
  expect_error(handle_id(NULL, data))

  # And verify that the waterfall works as expected
  ID <- handle_id(NULL, data[, 1:2])
  expect_equal(ID, "fab_ID_2")
})

test_that("Error handlers: handle_n", {

  e <- new.env()

  # working env now req'd
  expect_error(handle_n(N=10, FALSE))

  # Passed closure as N, didn't evaluate it
  expect_error(handle_n(N = function(x) {
    x * 2
  }, working_environment = e))

  # Passed function call as N, did evaluate it
  func <- function(x) {
    x * 2
  }
  handle_n(N = func(4), working_environment=e)

  # Non-numeric type where coercion gives a warning
  expect_error(handle_n(N = "hello", working_environment =e))

  # Non-numeric type where coercion gives an explicit error
  expect_error(handle_n(N = list(Z = Y ~ X), working_environment = e))
})

test_that("Error handlers: check_rectangular", {
  # Everything is either length N or length 1
  test <- list(
    X = 1:10,
    Y = 11:20,
    Z = 4
  )
  N <- 10
  rectangularized <- check_rectangular(test, N)
  coerced_df <- as.data.frame(rectangularized)
  expect_equal(nrow(coerced_df), 10)
  expect_equal(ncol(coerced_df), 3)

  test[["K"]] <- 5:7
  expect_error(check_rectangular(test, N))
})

test_that("Error handlers: check_rectangular nested structures", {

  sleep[["nested"]] <- list(sleep)
  N <- 20
  expect_true(is.data.frame(check_rectangular(sleep, N)))

  sleep <- as.list(sleep)
  sleep$mtcars <- mtcars

  expect_error(check_rectangular(sleep, N))
})


test_that("get_unique_variables_by_level", {
  df <- datasets::ChickWeight
  expect_equal(length(get_unique_variables_by_level(df, "Diet")), 0)

  df$DietVar <- as.numeric(df$Diet) * 3
  expect_equal(length(get_unique_variables_by_level(df, "Diet")), 1)
})

test_that("Advance lookahead symbol evaluator", {
  my_quos <- rlang::quos(J = KK * LMNOP * max(F, G, H, 20, (((K)))), Z = K)
  expect_equal(
    get_symbols_from_quosures(my_quos),
    c("KK", "LMNOP", "F", "G", "H", "K")
  )
})


test_that("check_variable_names works",{
  expect_error(fabricate(a=add_level(10), a=modify_level('b')), "<unnamed>")
})

context("Helper functions")

test_that("Error handlers: handle_data", {
  # User passed in data that isn't a data frame (no dimensionality)
  expect_error(handle_data(1:10))

  # It's not a data frame and the user didn't specify the argument name
  # this is a weird case involving arguments being interpreted by position
  # from fabricate and weird stuff flipping through to handle_data
  expect_error(handle_data(matrix(1:9, nrow=3, ncol=3)))

  # Test coercion of dimensional but non-df objects -- a matrix is most common
  # example -- this is also a working use case.
  handle_data(data = matrix(1:9, nrow=3, ncol=3))

  # Really stupid test -- object has dimensionality but won't coerce to df
  # this should almost never happen except for very poorly behaved objects
  X = 1:10
  Y = X*2 + rnorm(10)
  df = data.frame(X = X, Y=Y)
  model = lm(Y ~ X, df)
  dim(model) = c(3, 4) # This will break the model object, but fine for test
  expect_error(handle_data(data = model))
})

test_that("Error handlers: handle_id", {
  # Cartoon scenario where we're asked to generate an ID
  # but our 6 default variables are all taken
  data = data.frame(ID = 1:10,
                    fab_ID_1 = 11:20,
                    fab_ID_2 = 21:30,
                    fab_ID_3 = 31:40,
                    fab_ID_4 = 41:50,
                    fab_ID_5 = 51:60)
  expect_error(handle_id(NULL, data))

  # And verify that the waterfall works as expected
  ID = handle_id(NULL, data[, 1:2])
  expect_equal(ID, "fab_ID_2")
})

test_that("Error handlers: handle_n", {
  # Passed closure as N, didn't evaluate it
  expect_error(handle_n(N = function(x) { x*2 }))

  # Passed closure as N, did evaluate it
  func = function(x) { x*2 }
  handle_n(N = func(4))

  # Non-numeric type where coercion gives a warning
  expect_error(handle_n(N = "hello"))

  # Non-numeric type where coercion gives an explicit error
  expect_error(handle_n(N = list(Z = Y ~ X)))
})

test_that("Error handlers: check_rectangular", {
  # Everything is either length N or length 1
  test = list(X = 1:10,
              Y = 11:20,
              Z = 4)
  N = 10
  check_rectangular(test, N)

  test[["K"]] = 5:7
  expect_error(check_rectangular(test, N))
})

test_that("get_unique_variables_by_level", {
  df = datasets::ChickWeight
  expect_equal(length(get_unique_variables_by_level(df, "Diet")), 0)

  df$DietVar = as.numeric(df$Diet) * 3
  expect_equal(length(get_unique_variables_by_level(df, "Diet")), 1)
})

test_that("Advance lookahead symbol evaluator", {
  my_quos = rlang::quos(J = KK * LMNOP * max(F, G, H, 20, (((K)))))
  expect_equal(length(get_symbols_from_quosure(my_quos)[[1]]), 6)
})

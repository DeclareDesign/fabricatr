context("Helper functions")

test_that("Error handlers", {
  # User passed in data that isn't a data frame (no dimensionality)
  expect_error(handle_data(1:10))

  # Data has dimensionality
  #df = handle_data(matrix(1:9, nrow=3, ncol=3))
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

  func = function(x) { x*2 }
  handle_n(N = func(4))

  expect_error(handle_n(N = "hello"))
})

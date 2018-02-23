context("Fabricate")

test_that("Weird edge cases: importing", {
  # No one should ever call import_data directly, and import_data_list
  # will generate a non-null environment, but let's make sure import_data
  # can catch a null environment
  we <- import_data(mtcars)
  expect_equal(class(we), "environment")
  expect_equal(
    "data_frame_output_" %in% names(we),
    TRUE
  )
})

test_that("Weird edge cases: missingness in draw_binomial", {
  # Missing data in prob -> still runs.
  expect_warning(draw_binomial(c(rep(0.5, 5), NA), trials = 8))
})

test_that("Weird edge cases: Running level calls externally", {
  # Several of these calls would fail even if they were inside, but let's
  # verify they fail outside too.
  expect_error(add_level(N = 20, x = rnorm(N)))
  expect_error(nest_level(N = 20, x = rnorm(N)))
  expect_error(modify_level(N = 20, x = rnorm(N)))
  expect_error(cross_levels(N = 20, x = rnorm(N)))
  expect_error(link_levels(N = 20, x = rnorm(N)))

  # The weird case where it thinks the unnamed add level call is data.
  expect_error(fabricatr(add_level(N = 20, x = rnorm(N))))
})

test_that("Modify level naked passthrough.", {
  # This should never happen; no one should naked call modify_level_internal
  # and the passthrough from fabricate guarantees an ID_label. But still, let's
  # verify that error triggers.
  expect_error(modify_level_internal())
})

test_that("Add level naked passthrough.", {
  # This should never happen; no one should naked call add_level_internal
  # and the passthrough from fabricate guarantees an ID_label. But still, let's
  # verify that error triggers.
  expect_error(add_level_internal(N = 20))
})

test_that("Ragged nested data length", {
  # This can happen if you have a fixed length inner data.
  # Length of the k-level variables has gotta be either 15 or 3, not 6.
  expect_error(fabricate(
    j = add_level(N = 5, x = rnorm(N)),
    k = add_level(N = 3, y = 1:6)
  ))
})

test_that("Using recycle to fill out a vector", {
  recycle_test <- fabricate(N = 20, months = recycle(month.abb))
  expect_equal(recycle_test$months, c(month.abb, month.abb[1:8]))

  expect_error(fabricate(N = 20, months = month.abb))

  expect_error(recycle(month.abb))
})

test_that("Unnamed level call for DD passthrough", {
  # Did use a name for a passthrough
  valid_inline_name <- fabricate(data = NULL, add_level(N = 10,
                                                        ID_label="test"))
  expect_equal(nrow(valid_inline_name), 10)
  expect_equal(names(valid_inline_name), "test")

  # Didn't use a name for a passthrough
  expect_error(fabricate(data = NULL, add_level(N = 10)))
})

test_that("Check that level is deprecated", {
  expect_error(fabricate(test = level(N = 10)))
})

test_that("Variable of length 1 on an inner level, issue #88", {
  result <- fabricate(
    level_1 = add_level(N = 5),
    level_2 = add_level(N = 5, D0 = 0, D1 = 1)
  )

  expect_equal(nrow(result), 25)
  expect_equal(all(result$D0 == 0), TRUE)
  expect_equal(all(result$D1 == 1), TRUE)
})

test_that("ID_label = NA", {
  result <- fabricate(data.frame(test1 = rnorm(5)), test2=rnorm(N),
                      ID_label = NA)
  expect_equal(ncol(result), 2)
  expect_identical(colnames(result), c("test1", "test2"))

  result <- fabricate(data.frame(test1 = rnorm(5)), test2=rnorm(N),
                      ID_label = "zzz")
  expect_equal(ncol(result), 3)
  expect_identical(colnames(result), c("test1", "zzz", "test2"))
})

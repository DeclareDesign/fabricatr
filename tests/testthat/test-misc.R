context("Fabricate")

test_that("Weird edge cases: importing", {
  # No one should ever call import_data directly, and import_data_list
  # will generate a non-null environment, but let's make sure import_data
  # can catch a null environment
  we = import_data(mtcars)
  expect_equal(class(we), "environment")
  expect_equal("data_frame_output_" %in% names(we),
               TRUE)
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
  expect_error(cross_level(N = 20, x = rnorm(N)))

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
  expect_error(add_level_internal(N=20))
})

test_that("Ragged nested data length", {
  # This can happen if you have a fixed length inner data.
  # Length of the k-level variables has gotta be either 15 or 3, not 6.
  expect_error(fabricate(j = add_level(N=5, x=rnorm(N)),
                         k = add_level(N=3, y=1:6)))

})

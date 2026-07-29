make_clustered <- function() {
  fabricate(
    clusters = add_level(N = 4, g = rnorm(N)),
    units    = add_level(N = 3, y = rnorm(N))
  )
}

test_that("simple bootstrap returns the same number of rows", {
  df <- fabricate(N = 50, Y = rnorm(N))
  expect_equal(nrow(resample_data(df)), 50L)
})

test_that("cluster resampling draws whole clusters", {
  dat <- make_clustered()
  out <- resample_data(dat, N = c(clusters = 2))
  expect_equal(nrow(out), 6L)
  # Clusters are drawn with replacement, so one may appear twice; either way
  # each appearance brings all 3 of its units.
  expect_true(all(table(out$clusters) %% 3 == 0))
})

test_that("ALL passes every unit at a level through", {
  dat <- make_clustered()
  out <- resample_data(dat, N = c(clusters = ALL, units = 2))
  expect_equal(nrow(out), 8L)
  expect_setequal(unique(out$clusters), unique(dat$clusters))
})

test_that("unique_labels are <id>_<draw> with no leading separator", {
  dat <- make_clustered()
  out <- resample_data(dat, N = c(clusters = 3), unique_labels = TRUE)
  expect_true("clusters_unique" %in% names(out))
  expect_false(any(startsWith(out$clusters_unique, "_")))
  expect_match(out$clusters_unique, "^[0-9]+_[0-9]+$")
  # A cluster drawn twice gets two distinct labels, one per draw
  expect_equal(length(unique(out$clusters_unique)), 3L)
})

test_that("nested unique_labels nest the parent label without doubling", {
  dat <- make_clustered()
  out <- resample_data(dat, N = c(clusters = 2, units = 2),
                       unique_labels = TRUE)
  expect_false(any(grepl("__", out$units_unique, fixed = TRUE)))
  expect_false(any(startsWith(out$units_unique, "_")))
  # every inner label begins with its own parent's label
  expect_true(all(startsWith(out$units_unique, out$clusters_unique)))
})

test_that("unique-label columns are ordered outermost first, after the data", {
  dat <- make_clustered()
  out <- resample_data(dat, N = c(clusters = 2, units = 2),
                       unique_labels = TRUE)
  expect_equal(names(out),
               c("clusters", "g", "units", "y",
                 "clusters_unique", "units_unique"))
})

test_that("resample_data rejects unknown or mismatched level specifications", {
  dat <- make_clustered()
  expect_error(resample_data(dat, N = c(nope = 2)), "ID_labels not found")
  expect_error(resample_data(dat, N = c(clusters = 2), ID_labels = "clusters"),
               "not both")
})

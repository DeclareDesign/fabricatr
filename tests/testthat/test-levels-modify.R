test_that("modify_level adds columns to existing level", {
  df <- fabricate(
    N = 20,
    cluster = sample(1:4, N, replace = TRUE),
    Y = rnorm(N),
    updated = modify_level(Y2 = Y * 2)
  )
  expect_true("Y2" %in% names(df))
  expect_equal(df$Y2, df$Y * 2)
})

test_that("modify_level with .by does grouped operation", {
  df <- fabricate(
    N = 20,
    g = rep(1:4, each = 5),
    Y = rnorm(N),
    upd = modify_level(gm = mean(Y), .by = "g")
  )
  # All rows in the same group should have the same gm
  expect_equal(
    df$gm[df$g == 1],
    rep(mean(df$Y[df$g == 1]), 5)
  )
})

test_that("modify_level named after a level evaluates once per unit of it", {
  # Matches fabricatr 1.0.2: 3 regions give 3 draws of z and N of 3, and a2
  # is built from the region's own a. Unlike 1.0.2, the cities stay in the
  # frame, with each region's values written to every city in it.
  set.seed(1)
  df <- fabricate(
    regions = add_level(N = 3, a = 1:3),
    cities  = add_level(N = 2, b = rnorm(N)),
    regions = modify_level(z = rnorm(N), a2 = a * 2, n_regions = N, k = n())
  )
  expect_equal(nrow(df), 6L)
  expect_equal(names(df), c("regions", "a", "cities", "b", "z", "a2",
                            "n_regions", "k"))
  expect_length(unique(df$z), 3L)
  expect_equal(df$z[1:2], rep(df$z[1], 2))
  expect_equal(df$a2, df$a * 2)
  expect_equal(unique(df$n_regions), 3L)
  expect_equal(unique(df$k), 3L)
})

test_that("a column from a level nested inside is out of view, as in 1.0.2", {
  expect_error(
    fabricate(
      regions = add_level(N = 3, a = 1:3),
      cities  = add_level(N = 2, b = rnorm(N)),
      regions = modify_level(bb = mean(b))
    ),
    "`b` is out of view inside `regions = modify_level()`",
    fixed = TRUE
  )
  # The grouped spelling is the way to summarise it
  df <- fabricate(
    regions = add_level(N = 3, a = 1:3),
    cities  = add_level(N = 2, b = rnorm(N)),
    modify_level(bb = mean(b), .by = "regions")
  )
  expect_equal(df$bb[1], mean(df$b[1:2]))
})

test_that("modify_level at the lowest level sees every column and every row", {
  df <- fabricate(
    regions = add_level(N = 3, a = 1:3),
    cities  = add_level(N = 2, b = 1:N),
    cities  = modify_level(c = b + a, n_cities = N)
  )
  expect_equal(df$c, df$b + df$a)
  expect_equal(unique(df$n_cities), 6L)
})

test_that("modify_level in the middle of three levels works at that level", {
  set.seed(2)
  df <- fabricate(
    a = add_level(N = 2, x = rnorm(N)),
    b = add_level(N = 2, y = rnorm(N)),
    c = add_level(N = 2, w = rnorm(N)),
    b = modify_level(z = rnorm(N), nb = N, x2 = x * 2)
  )
  expect_equal(nrow(df), 8L)
  expect_length(unique(df$z), 4L)
  expect_equal(unique(df$nb), 4L)
  expect_equal(df$x2, df$x * 2)
  expect_equal(df$z[c(1, 3, 5, 7)], df$z[c(2, 4, 6, 8)])
})

test_that("modify_level requires one value per unit of the level", {
  expect_error(
    fabricate(g = add_level(N = 3, x = 1:3), g = modify_level(y = c(1, 2))),
    "returned 2 values for 3 g")
})

test_that("a genuine missing object is reported as itself", {
  # The out-of-view message rewrites "object 'x' not found" only when `x` is a
  # real column of a level nested inside, so a plain typo must pass through.
  expect_error(
    fabricate(g = add_level(N = 3, x = 1:3), g = modify_level(y = notacolumn * 2)),
    "object 'notacolumn' not found")
})

test_that("modify_level(.by = ) names the argument when it is not a column name", {
  # `.by` is an ordinary argument, so a bare column name evaluates to the
  # column and arrives as a vector; the 1.x spelling `by = ` is captured
  # unevaluated and does take a bare name, so the two differ and the error
  # has to say which one this is. `lst[[by]]` used to fail inside the
  # subscript, with "no such index at level 1" for a bare name, "argument 1
  # is not a vector" for a column that is not there, and "subscript out of
  # bounds" for two names, while a number silently grouped by whichever
  # column came first.
  expect_error(fabricate(g = add_level(N = 2, a = 1:2),
                         u = add_level(N = 3, Y = rnorm(N)),
                         u = modify_level(m = mean(Y), .by = g)),
               "takes one column name, written as a string")
  expect_error(modify_level(m = 1, .by = 1), "written as a string")
  expect_error(modify_level(m = 1, .by = c("g", "u")), "written as a string")
  expect_error(modify_level(m = 1, .by = NA_character_), "written as a string")

  expect_error(fabricate(g = add_level(N = 2, a = 1:2),
                         u = add_level(N = 3, Y = rnorm(N)),
                         u = modify_level(m = mean(Y), .by = "nope")),
               "no column named `nope` is in view")
})

test_that("both spellings of the grouping column still work", {
  by_string <- fabricate(g = add_level(N = 2, a = 1:2),
                         u = add_level(N = 3, Y = rnorm(N)),
                         u = modify_level(m = mean(Y), .by = "g"))
  by_legacy <- suppressWarnings(
    fabricate(g = add_level(N = 2, a = 1:2),
              u = add_level(N = 3, Y = rnorm(N)),
              u = modify_level(m = mean(Y), by = g)))
  expect_equal(nrow(by_string), 6L)
  expect_length(unique(by_string$m), 2L)
  expect_equal(names(by_legacy), names(by_string))
})

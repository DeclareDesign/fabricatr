# fabricatr#165: an existing data frame becomes a level, keeping its own key.

ind <- data.frame(individuals = c("ann", "bob", "cyd"),
                  ind_shock = c(-0.4, 0.1, 0.8),
                  stringsAsFactors = FALSE)
per <- data.frame(year = 2020:2022, period_shock = c(0.2, -0.1, 0.3))

# What the level is made of -------------------------------------------------

test_that("the imported rows and columns arrive intact", {
  df <- fabricate(units = import_level(ind))
  expect_equal(nrow(df), nrow(ind))
  expect_equal(df$individuals, ind$individuals)
  expect_equal(df$ind_shock, ind$ind_shock)
  # The ID is added, nothing is taken away
  expect_equal(names(df), c("units", "individuals", "ind_shock"))
})

test_that("importing does not modify the data frame it was given", {
  before <- ind
  invisible(fabricate(individuals = import_level(ind, z = ind_shock + 1)))
  expect_identical(ind, before)
})

test_that("importing consumes no random numbers", {
  set.seed(343)
  direct <- rnorm(3)
  set.seed(343)
  imported <- fabricate(u = import_level(data.frame(x = 1:3)), y = rnorm(N))$y
  expect_identical(imported, direct)
})

test_that("a data frame, a tibble, and a grouped tibble import alike", {
  base_df <- data.frame(u = c("a", "b"), x = 1:2)
  expect_equal(fabricate(u = import_level(base_df))$x, 1:2)
  expect_equal(fabricate(u = import_level(tibble::as_tibble(base_df)))$x, 1:2)
  grouped <- dplyr::group_by(tibble::as_tibble(base_df), u)
  out <- fabricate(u = import_level(grouped))
  expect_equal(out$x, 1:2)
  expect_false(dplyr::is_grouped_df(out))
})

test_that("a one-row frame and a frame with no columns are both levels", {
  expect_equal(nrow(fabricate(u = import_level(data.frame(u = "only", x = 1)))), 1L)
  bare <- fabricate(u = import_level(data.frame(row.names = 1:3)))
  expect_equal(names(bare), "u")
  expect_equal(bare$u, c("1", "2", "3"))
})

# Which column is the ID ----------------------------------------------------

test_that("a column named after the level is the ID without being asked", {
  df <- fabricate(individuals = import_level(ind))
  expect_equal(names(df), c("individuals", "ind_shock"))
  expect_equal(df$individuals, ind$individuals)
})

test_that(".id moves its column to the front under the level's name", {
  df <- fabricate(period = import_level(per, .id = "year"))
  expect_equal(names(df), c("period", "period_shock"))
  expect_equal(df$period, per$year)
  expect_false("year" %in% names(df))
})

test_that(".id naming the column the level is already named after is a no-op", {
  expect_identical(fabricate(individuals = import_level(ind, .id = "individuals")),
                   fabricate(individuals = import_level(ind)))
})

test_that("a frame with no key of its own gets numbered rows", {
  df <- fabricate(firms = import_level(data.frame(rev = c(10, 20, 30))))
  expect_equal(df$firms, c("1", "2", "3"))
  expect_equal(df$rev, c(10, 20, 30))
  # Padded to the number of units, as every other level's ID is
  wide <- fabricate(firms = import_level(data.frame(rev = seq_len(12))))
  expect_equal(wide$firms[1:2], c("01", "02"))
})

test_that("an imported key keeps its own type", {
  chr <- fabricate(individuals = import_level(ind))
  expect_type(chr$individuals, "character")

  int <- fabricate(period = import_level(per, .id = "year"))
  expect_type(int$period, "integer")

  dates <- data.frame(day = as.Date("2020-01-01") + 0:2, v = 1:3)
  expect_s3_class(fabricate(day = import_level(dates))$day, "Date")

  fct <- data.frame(g = factor(c("lo", "hi"), levels = c("lo", "hi")), v = 1:2)
  out <- fabricate(g = import_level(fct))$g
  expect_s3_class(out, "factor")
  expect_equal(levels(out), c("lo", "hi"))
})

test_that("an ID that does not identify a unit is refused", {
  expect_error(
    fabricate(g = import_level(data.frame(g = c("a", "a", "b"), x = 1:3))),
    "1 of its values appears more than once"
  )
  expect_error(
    fabricate(g = import_level(data.frame(g = c("a", "a", "b", "b"), x = 1:4))),
    "2 of its values appear more than once"
  )
  expect_error(
    fabricate(g = import_level(data.frame(k = c(1, 1, 2), x = 1:3), .id = "k")),
    "leave it off to have fabricatr number the rows"
  )
  expect_error(
    fabricate(g = import_level(data.frame(k = c(1, NA, 2), x = 1:3), .id = "k")),
    "is missing, and a missing ID identifies nothing"
  )
})

test_that("import_level refuses what it cannot make a level of", {
  expect_error(fabricate(g = import_level(1:10)), "class integer")
  expect_error(fabricate(g = import_level(matrix(1:4, 2))), "class matrix")
  expect_error(fabricate(g = import_level(ind[0, ])), "no rows")
  expect_error(fabricate(g = import_level(ind, .id = "nope")),
               "no column called `nope`")
  expect_error(fabricate(g = import_level(ind, .id = 1)), "names one column")
  expect_error(fabricate(g = import_level(ind, .id = c("a", "b"))),
               "names one column")
  expect_error(fabricate(g = import_level(ind, .id = NA)), "names one column")
  expect_error(fabricate(import_level(ind, .id = "individuals")),
               "no level name")
  expect_error(
    fabricate(period = import_level(
      data.frame(period = c("p1", "p2"), year = 2020:2021), .id = "year")),
    "already has a column called `period`"
  )
})

test_that("a column called N is refused rather than silently dropped", {
  sites <- data.frame(site = c("a", "b"), N = c(50, 70))
  expect_error(fabricate(site = import_level(sites)),
               "column called `N`")
  # The escape the message names still works
  expect_equal(fabricate(data = sites, doubled = N * 2)$doubled, c(100, 140))
})

# Expressions inside the level ----------------------------------------------

test_that("expressions see the imported columns, N, and n()", {
  df <- fabricate(units = import_level(ind, k = N, kk = n(),
                                       doubled = ind_shock * 2))
  expect_equal(df$k, rep(3L, 3))
  expect_equal(df$kk, rep(3L, 3))
  expect_equal(df$doubled, ind$ind_shock * 2)
})

test_that("expressions are evaluated in order, each seeing the last", {
  df <- fabricate(u = import_level(data.frame(x = 1:3), a = x * 2, b = a + 1))
  expect_equal(df$b, c(3, 5, 7))
})

test_that("a length-1 expression fills the level", {
  expect_equal(fabricate(u = import_level(data.frame(x = 1:3), k = "same"))$k,
               rep("same", 3))
})

test_that("the issue's single-frame case builds", {
  df <- data.frame(x = c(10, 12, 9, 15), y = 1:4)
  out <- fabricate(times = import_level(df, z = x - dplyr::lag(x, 1)))
  expect_equal(out$z, c(NA, 2, -3, 6))
  expect_equal(out$times, c("1", "2", "3", "4"))
})

test_that("import_level works from inside a function that holds the data", {
  f <- function(d) fabricate(u = import_level(d, y = x * 2))
  expect_equal(f(data.frame(x = 1:3))$y, c(2, 4, 6))
})

test_that("potential outcomes declared on an imported level reveal", {
  df <- fabricate(
    units = import_level(ind, potential_outcomes(Y ~ 0.5 * Z + ind_shock)),
    Z = rep(c(0, 1), length.out = N),
    Y = reveal_outcomes(Y ~ Z)
  )
  expect_equal(df$Y, ifelse(df$Z == 1, df$Y_Z_1, df$Y_Z_0))
})

# Levels built on top of an imported one ------------------------------------

test_that("fabricated levels nest inside an imported one", {
  df <- fabricate(
    villages = import_level(data.frame(villages = c("v1", "v2"),
                                       v_income = c(10, 12))),
    citizens = nest_level(N = 3, income = v_income)
  )
  expect_equal(nrow(df), 6L)
  expect_equal(df$villages, rep(c("v1", "v2"), each = 3))
  expect_equal(df$citizens, as.character(1:6))
  expect_equal(df$income, rep(c(10, 12), each = 3))
})

test_that("an imported hierarchy resamples by its own level names", {
  set.seed(343)
  df <- fabricate(
    villages = import_level(data.frame(villages = c("v1", "v2"),
                                       v_income = c(10, 12))),
    citizens = nest_level(N = 3)
  )
  boot <- resample_data(df, N = c(villages = 2, citizens = 2))
  expect_equal(nrow(boot), 4L)
  expect_true(all(boot$villages %in% c("v1", "v2")))
})

test_that("an imported level can be modified at its own level", {
  df <- fabricate(
    individuals = import_level(ind),
    individuals = modify_level(big = ind_shock > 0)
  )
  expect_equal(df$big, c(FALSE, TRUE, TRUE))
})

# Crossing and linking, which is what the issue asked for --------------------

test_that("two imported frames cross, each keeping its own key", {
  df <- fabricate(
    individuals = import_level(ind),
    period      = import_level(per, .id = "year"),
    obs         = cross_levels(.by = c("individuals", "period"),
                               Y = ind_shock + period_shock)
  )
  expect_equal(nrow(df), 9L)
  expect_equal(names(df),
               c("individuals", "ind_shock", "period", "period_shock", "obs", "Y"))
  expect_equal(df$Y, df$ind_shock + df$period_shock)
  # Every combination exactly once, first level varying fastest
  expect_equal(df$individuals, rep(ind$individuals, times = 3))
  expect_equal(df$period, rep(per$year, each = 3))
  expect_equal(nrow(dplyr::distinct(df, individuals, period)), 9L)
})

test_that("an imported level crosses with a fabricated one", {
  df <- fabricate(
    a   = add_level(N = 2, p = 1:2),
    b   = import_level(data.frame(b = c("x", "y"), q = 3:4)),
    obs = cross_levels(.by = c("a", "b"))
  )
  expect_equal(nrow(df), 4L)
  expect_equal(df$b, rep(c("x", "y"), each = 2))
  expect_equal(df$p, rep(1:2, times = 2))
})

test_that("a column modified at an imported level survives the crossing", {
  df <- fabricate(
    individuals = import_level(ind),
    individuals = modify_level(big = ind_shock > 0),
    period      = import_level(per, .id = "year"),
    obs         = cross_levels(.by = c("individuals", "period"))
  )
  expect_equal(df$big, rep(c(FALSE, TRUE, TRUE), times = 3))
})

test_that("imported levels link as declared ones do", {
  set.seed(343)
  df <- fabricate(
    individuals = import_level(ind),
    period      = import_level(per, .id = "year"),
    obs         = link_levels(N = 100, .by = c("individuals", "period"),
                              rho = 0.5)
  )
  expect_equal(nrow(df), 100L)
  expect_true(all(df$period %in% per$year))
  expect_true(all(df$individuals %in% ind$individuals))
})

test_that("two imported frames sharing a column name are refused by name", {
  expect_error(
    fabricate(
      a   = import_level(data.frame(a = 1:2, v = 1:2)),
      b   = import_level(data.frame(b = 1:2, v = 3:4)),
      obs = cross_levels(.by = c("a", "b"))
    ),
    "both carry `v`"
  )
})

test_that("an unnamed import_level contributes its rows without an ID", {
  df <- fabricate(import_level(data.frame(x = 1:3)))
  expect_equal(names(df), "x")
  expect_equal(nrow(df), 3L)
})

test_that("an imported level reaches DeclareDesign's entry point", {
  dots <- rlang::quos(
    individuals = import_level(ind),
    period      = import_level(per, .id = "year"),
    obs         = cross_levels(.by = c("individuals", "period"),
                               Y = ind_shock + period_shock)
  )
  df <- fabricatr:::fabricate_with_dots(dots = dots)
  expect_equal(nrow(df), 9L)
  expect_equal(df$Y, df$ind_shock + df$period_shock)
})

# The spelling the issue was filed on ---------------------------------------

test_that("a missing N names import_level when data was passed to a level", {
  df <- data.frame(x = rnorm(10), y = 1:10)
  expect_error(fabricate(times = add_level(data = df, z = x * 2)),
               "import_level\\(df, \\.\\.\\.\\)")
  expect_error(fabricate(times = declare_level(data = df)),
               "import_level\\(df, \\.\\.\\.\\)")
  expect_error(fabricate(a = add_level(x = 1:3)),
               "needs `N`, the number of rows the level builds")
  expect_error(fabricate(a = add_level(N = 2), b = nest_level(x = 1)),
               "for each row in hand")
  # A column that happens to be called `data` is still a column
  expect_equal(fabricate(a = add_level(N = 2, data = c(1, 2)))$data, c(1, 2))
  # And an expression called `data` that is not a data frame says only that
  # `N` is missing
  expect_error(fabricate(a = add_level(data = rnorm(3))), "needs `N`")
})

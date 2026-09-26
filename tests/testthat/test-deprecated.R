# fabricatr's spellings, accepted with a warning that shows the rewrite.

test_that("nest = FALSE builds an independent level, and says what to write", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(
    step <- add_level(N = 2, time_trend = 1:N, nest = FALSE),
    "`nest = FALSE` is deprecated"
  )
  expect_equal(step$type, "declare")
  expect_false("nest" %in% names(step$dots))

  msg <- tryCatch(add_level(N = 2, time_trend = 1:N, nest = FALSE),
                  warning = conditionMessage)
  expect_match(msg, "Write:  declare_level(N = 2, time_trend = 1:N)",
               fixed = TRUE)
  expect_match(msg, "Not:    add_level(N = 2, time_trend = 1:N, nest = FALSE)",
               fixed = TRUE)
})

test_that("nest = TRUE is add_level with the argument dropped", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  msg <- tryCatch(add_level(N = 5, x = 1, nest = TRUE),
                  warning = conditionMessage)
  expect_match(msg, "Write:  add_level(N = 5, x = 1)", fixed = TRUE)
  step <- suppressWarnings(add_level(N = 5, x = 1, nest = TRUE))
  expect_equal(step$type, "add")
})

test_that("by = is accepted and rewritten to .by in place", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  msg <- tryCatch(cross_levels(by = join_using(countries, years), Y = 1),
                  warning = conditionMessage)
  expect_match(msg, 'Write:  cross_levels(.by = c("countries", "years"), Y = 1)',
               fixed = TRUE)
  step <- suppressWarnings(cross_levels(by = join_using(countries, years), Y = 1))
  expect_equal(step$by, c("countries", "years"))
  expect_false("by" %in% names(step$dots))
})

test_that("by = takes a bare name, a join_using() call, and a character vector", {
  expect_equal(suppressWarnings(modify_level(m = 1, by = clusters))$by,
               "clusters")
  expect_equal(suppressWarnings(cross_levels(by = c("a", "b")))$by, c("a", "b"))
  expect_equal(suppressWarnings(link_levels(N = 5, by = join_using(a, b)))$by,
               c("a", "b"))
})

test_that(".by keeps working and stays silent", {
  expect_no_warning(step <- cross_levels(.by = c("a", "b"), Y = 1))
  expect_equal(step$by, c("a", "b"))
  expect_no_warning(add_level(N = 3, x = 1))
})

test_that("the panel idiom runs as fabricatr writes it", {
  set.seed(1)
  out <- suppressWarnings(fabricate(
    countries = add_level(N = 3, country_shock = rnorm(N)),
    years     = add_level(N = 2, time_trend = 1:N, nest = FALSE),
    observation = cross_levels(by = join_using(countries, years),
                               Y = country_shock + time_trend)
  ))
  set.seed(1)
  ported <- fabricate(
    countries = declare_level(N = 3, country_shock = rnorm(N)),
    years     = declare_level(N = 2, time_trend = 1:N),
    observation = cross_levels(.by = c("countries", "years"),
                               Y = country_shock + time_trend)
  )
  expect_equal(out, ported)
  expect_equal(nrow(out), 6L)
})

test_that("join_using names levels and is silent in front of .by", {
  expect_equal(join_using(countries, years), c("countries", "years"))
  expect_equal(join_using("a", "b"), c("a", "b"))
  expect_no_warning(cross_levels(.by = join_using(a, b)))
})

test_that("recycle fills a level and checks divisibility", {
  expect_equal(recycle(c("a", "b"), .N = 6),
               c("a", "b", "a", "b", "a", "b"))
  expect_error(recycle(c("a", "b"), .N = 5), "do not divide")
  out <- fabricate(villages = add_level(N = 6, arm = recycle(c("a", "b", "c"))))
  expect_equal(out$arm, c("a", "b", "c", "a", "b", "c"))
})

test_that("a column really named nest or by is still reachable", {
  # The shims cost this: `nest` and `by` can no longer be column names in a
  # level constructor. Both are formals in fabricatr too, so no design written
  # for either package could have used them, and the warning says what happened.
  out <- fabricate(N = 3, nest = c(1, 2, 3), by = c(4, 5, 6))
  expect_equal(out$nest, c(1, 2, 3))
  expect_equal(out$by, c(4, 5, 6))
})

test_that("rho inside join_using() reaches link_levels, and the rewrite shows it", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  msg <- tryCatch(link_levels(N = 500, by = join_using(a, b, rho = 0.5)),
                  warning = conditionMessage)
  expect_match(msg, 'Write:  link_levels(N = 500, .by = c("a", "b"), rho = 0.5)',
               fixed = TRUE)
  step <- suppressWarnings(link_levels(N = 500, by = join_using(a, b, rho = 0.5)))
  expect_equal(step$by, c("a", "b"))
  expect_equal(step$rho, 0.5)

  set.seed(4)
  df <- suppressWarnings(fabricate(
    a   = declare_level(N = 50),
    b   = declare_level(N = 50),
    obs = link_levels(N = 500, by = join_using(a, b, rho = 0.8))
  ))
  expect_gt(cor(as.numeric(df$a), as.numeric(df$b), method = "spearman"), 0.6)
})

test_that("break_labels and category_labels are accepted as labels", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  set.seed(3)
  x <- rnorm(5)
  msg <- tryCatch(draw_ordered(x, breaks = c(-1, 0, 1),
                               break_labels = c("a", "b", "c", "d")),
                  warning = conditionMessage)
  expect_match(msg, "`break_labels =` is deprecated", fixed = TRUE)
  expect_match(msg, 'Write:  draw_ordered(x, breaks = c(-1, 0, 1), labels = c("a", "b", "c", "d"))',
               fixed = TRUE)
  old <- suppressWarnings(draw_ordered(x, breaks = c(-1, 0, 1),
                                       break_labels = c("a", "b", "c", "d")))
  expect_equal(old, draw_ordered(x, breaks = c(-1, 0, 1),
                                 labels = c("a", "b", "c", "d")))
  expect_true(is.ordered(old))

  msg <- tryCatch(draw_categorical(N = 5, prob = c(0.2, 0.5, 0.3),
                                   category_labels = c("lo", "mid", "hi")),
                  warning = conditionMessage)
  expect_match(msg, "`category_labels =` is deprecated", fixed = TRUE)
  expect_match(msg, 'Write:  draw_categorical(N = 5, prob = c(0.2, 0.5, 0.3), labels = c("lo", "mid", "hi"))',
               fixed = TRUE)
  set.seed(3)
  old <- suppressWarnings(draw_categorical(N = 5, prob = c(0.2, 0.5, 0.3),
                                           category_labels = c("lo", "mid", "hi")))
  set.seed(3)
  expect_equal(old, draw_categorical(N = 5, prob = c(0.2, 0.5, 0.3),
                                     labels = c("lo", "mid", "hi")))
  expect_s3_class(old, "factor")
  expect_false(is.ordered(old))
})

test_that("recycle() says how to supply N when it cannot find one", {
  expect_error(fabricatr:::recycle(1:2), "could not find `N`")
})

test_that("legacy `by =` does not eat the first unnamed argument", {
  # R will not partial-match `by =` to the `.by` formal, so `.by` is left
  # unfilled and the first unnamed argument binds to it positionally. Before
  # this was handled, `absorb_legacy_by()` returned the legacy value and the
  # unnamed argument vanished without a warning.
  set.seed(343)
  legacy <- suppressWarnings(fabricate(
    units   = declare_level(N = 3, u = rnorm(N)),
    periods = declare_level(N = 2, p = rnorm(N)),
    obs     = cross_levels(by = join_using(units, periods),
                           potential_outcomes(Y ~ Z), Z = 1)
  ))
  set.seed(343)
  modern <- fabricate(
    units   = declare_level(N = 3, u = rnorm(N)),
    periods = declare_level(N = 2, p = rnorm(N)),
    obs     = cross_levels(.by = c("units", "periods"),
                           potential_outcomes(Y ~ Z), Z = 1)
  )
  expect_equal(legacy, modern)
  expect_true(all(c("Y_Z_0", "Y_Z_1") %in% names(legacy)))

  set.seed(343)
  legacy_link <- suppressWarnings(fabricate(
    a  = declare_level(N = 4, x = rnorm(N)),
    b  = declare_level(N = 4, y = rnorm(N)),
    ab = link_levels(N = 6, by = join_using(a, b),
                     potential_outcomes(Y ~ Z), Z = 1)
  ))
  set.seed(343)
  modern_link <- fabricate(
    a  = declare_level(N = 4, x = rnorm(N)),
    b  = declare_level(N = 4, y = rnorm(N)),
    ab = link_levels(N = 6, .by = c("a", "b"),
                     potential_outcomes(Y ~ Z), Z = 1)
  )
  expect_equal(legacy_link, modern_link)
})

test_that("naming both `.by` and `by` displaces nothing", {
  set.seed(343)
  both <- suppressWarnings(fabricate(
    units   = declare_level(N = 3, u = rnorm(N)),
    periods = declare_level(N = 2, p = rnorm(N)),
    obs     = cross_levels(.by = c("units", "periods"),
                           by = join_using(units, periods), Y = u + p)
  ))
  expect_equal(names(both), c("units", "u", "periods", "p", "obs", "Y"))
})

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

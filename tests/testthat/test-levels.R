test_that("add_level creates correct row count and ID column", {
  df <- fabricate(villages = add_level(N = 10, income = rnorm(N)))
  expect_equal(nrow(df), 10L)
  expect_true("villages" %in% names(df))
  expect_true("income" %in% names(df))
  expect_false("N" %in% names(df))
})

test_that("nest_level fans out rows correctly (scalar N)", {
  df <- fabricate(
    villages = add_level(N = 5, v_inc = rnorm(N)),
    citizens = nest_level(N = 10, c_inc = v_inc + rnorm(N))
  )
  expect_equal(nrow(df), 50L)
  expect_true(all(c("villages", "citizens", "v_inc", "c_inc") %in% names(df)))
})

test_that("nest_level uses inner N, not outer N, in expressions", {
  df <- fabricate(
    blocks = add_level(N = 3),
    units  = nest_level(N = 4, Z = rep(0:1, N / 2))
  )
  # rep(0:1, 2) = c(0,1,0,1) per block -> 12 total rows
  expect_equal(nrow(df), 12L)
  expect_setequal(df$Z, c(0L, 1L))
})

test_that("nest_level supports variable per-parent N", {
  df <- fabricate(
    countries = add_level(N = 3, n_cities = c(2L, 3L, 4L)),
    cities    = nest_level(N = n_cities, gdp = rnorm(N))
  )
  expect_equal(nrow(df), 9L)  # 2+3+4
})

test_that("declare_level and cross_levels produce Cartesian product", {
  df <- fabricate(
    countries = declare_level(N = 4, gdp = runif(N, 1, 10)),
    years     = declare_level(N = 3, shock = runif(N, 0, 1)),
    obs       = cross_levels(.by = c("countries", "years"),
                             Y = gdp + shock)
  )
  expect_equal(nrow(df), 12L)
  expect_true("Y" %in% names(df))
})

test_that("cross_levels errors on missing level name", {
  expect_error(
    fabricate(
      A = declare_level(N = 3),
      obs = cross_levels(.by = c("A", "B"))
    ),
    "not found in registry"
  )
})

test_that("link_levels samples N rows from cross product", {
  df <- fabricate(
    primary   = declare_level(N = 10, pq = runif(N)),
    secondary = declare_level(N = 8,  sq = runif(N)),
    students  = link_levels(N = 50, .by = c("primary", "secondary"),
                            score = pq + sq)
  )
  expect_equal(nrow(df), 50L)
  expect_true("score" %in% names(df))
})

test_that("link_levels with rho produces correlated assignments", {
  set.seed(42)
  df <- fabricate(
    A = declare_level(N = 100, a_val = seq(0, 1, length.out = N)),
    B = declare_level(N = 100, b_val = seq(0, 1, length.out = N)),
    obs = link_levels(N = 500, .by = c("A", "B"), rho = 0.8,
                      x = a_val + b_val)
  )
  expect_equal(nrow(df), 500L)
  # Positive rho: units with high a_val should tend to pair with high b_val
  expect_gt(cor(df$a_val, df$b_val, method = "spearman"), 0.4)
})

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

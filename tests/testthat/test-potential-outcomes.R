test_that("potential_outcomes creates two columns for binary treatment", {
  df <- fabricate(
    N = 20, U = rnorm(N),
    potential_outcomes(Y ~ 0.5 * Z + U)
  )
  expect_true("Y_Z_0" %in% names(df))
  expect_true("Y_Z_1" %in% names(df))
  expect_equal(df$Y_Z_0, df$U)
  expect_equal(df$Y_Z_1, 0.5 + df$U)
})

test_that("potential_outcomes works with three conditions", {
  df <- fabricate(
    N = 10,
    potential_outcomes(Y ~ Z * 2, conditions = list(Z = 0:2))
  )
  expect_true(all(c("Y_Z_0", "Y_Z_1", "Y_Z_2") %in% names(df)))
  expect_equal(df$Y_Z_2, rep(4, 10))
})

test_that("potential_outcomes supports multi-arm factorial", {
  df <- fabricate(
    N = 8,
    potential_outcomes(Y ~ Z1 + Z2, conditions = list(Z1 = 0:1, Z2 = 0:1))
  )
  expect_equal(length(grep("^Y_", names(df))), 4L)
})

test_that("reveal_outcomes selects correct column per unit", {
  dat <- fabricate(
    N = 100, U = rnorm(N),
    potential_outcomes(Y ~ 0.5 * Z + U)
  )
  dat <- fabricate(
    data = dat,
    Z = rbinom(N, 1, 0.5),
    Y = reveal_outcomes(Y ~ Z)
  )
  expect_equal(dat$Y[dat$Z == 0], dat$Y_Z_0[dat$Z == 0])
  expect_equal(dat$Y[dat$Z == 1], dat$Y_Z_1[dat$Z == 1])
})

test_that("reveal_outcomes works with factorial treatments", {
  dat <- fabricate(
    N = 50,
    potential_outcomes(Y ~ Z1 * 0.3 + Z2 * 0.5,
                       conditions = list(Z1 = 0:1, Z2 = 0:1))
  )
  dat <- fabricate(
    data = dat,
    Z1 = rbinom(N, 1, 0.5),
    Z2 = rbinom(N, 1, 0.5),
    Y  = reveal_outcomes(Y ~ Z1 + Z2)
  )
  expect_length(dat$Y, 50L)
})

test_that("reveal_outcomes keeps the type of the potential outcomes", {
  # 1.0.2, and every 2.0 build before this one, selected the revealed value by
  # matrix-indexing the data frame of potential outcome columns, which goes
  # through as.matrix() and renders anything non-numeric as character. This is
  # a deliberate break with 1.0.2: an ordered outcome lost its levels, so
  # re-factoring what came back ordered them alphabetically and any model
  # fitted on it used the wrong baseline without saying so.
  d <- fabricate(
    N = 6, U = rnorm(N),
    potential_outcomes(Y ~ draw_ordered(0.5 * Z + U, breaks = c(-1, 1),
                                        labels = c("lo", "mid", "hi")))
  )
  revealed <- fabricate(data = d, Z = rep(0:1, 3),
                        Yobs = reveal_outcomes(Y ~ Z))$Yobs

  expect_s3_class(revealed, "ordered")
  expect_identical(levels(revealed), c("lo", "mid", "hi"))
  expect_identical(as.character(revealed),
                   ifelse(rep(0:1, 3) == 1, as.character(d$Y_Z_1),
                          as.character(d$Y_Z_0)))

  # A Date degraded to a string the same way.
  dates <- fabricate(N = 4, potential_outcomes(Y ~ as.Date("2026-01-01") + Z))
  revealed_date <- fabricate(data = dates, Z = c(0, 1, 0, 1),
                             Yobs = reveal_outcomes(Y ~ Z))$Yobs
  expect_s3_class(revealed_date, "Date")
  expect_identical(revealed_date,
                   as.Date(c("2026-01-01", "2026-01-02",
                             "2026-01-01", "2026-01-02")))

  # The types that were already right stay right.
  num <- fabricate(N = 6, U = rnorm(N), potential_outcomes(Y ~ 0.5 * Z + U))
  z <- rep(0:1, 3)
  expect_identical(
    fabricate(data = num, Z = z, Yobs = reveal_outcomes(Y ~ Z))$Yobs,
    ifelse(z == 1, num$Y_Z_1, num$Y_Z_0)
  )

  lgl <- fabricate(N = 4, potential_outcomes(Y ~ Z > 0))
  expect_type(fabricate(data = lgl, Z = c(0, 1, 0, 1),
                        Yobs = reveal_outcomes(Y ~ Z))$Yobs, "logical")
})

test_that("conditions that disagree on factor levels fall back and say so", {
  # There is no set of levels to keep, and inventing an order across level sets
  # that were never meant to be compared is worse than returning character.
  d <- fabricate(
    N = 4,
    potential_outcomes(Y ~ factor(ifelse(Z > 0, "a", "b"),
                                  levels = if (Z[1] > 0) c("a", "b") else c("b", "a")))
  )
  expect_warning(
    revealed <- fabricate(data = d, Z = c(0, 1, 0, 1),
                          Yobs = reveal_outcomes(Y ~ Z))$Yobs,
    "do not share one set of factor levels"
  )
  expect_type(revealed, "character")
  expect_identical(revealed, c("b", "a", "b", "a"))
})

test_that("a factorial reveal picks the right cell and keeps its type", {
  d <- fabricate(
    N = 8, U = rnorm(N),
    potential_outcomes(Y ~ factor(ifelse(Z1 + Z2 > 0, "any", "none"),
                                  levels = c("none", "any")),
                       conditions = list(Z1 = 0:1, Z2 = 0:1))
  )
  z1 <- rep(0:1, 4)
  z2 <- rep(c(0, 0, 1, 1), 2)
  revealed <- fabricate(data = d, Z1 = z1, Z2 = z2,
                        Yobs = reveal_outcomes(Y ~ Z1 + Z2))$Yobs

  expect_s3_class(revealed, "factor")
  expect_identical(levels(revealed), c("none", "any"))
  wanted <- vapply(seq_len(8), function(i) {
    as.character(d[[paste0("Y_Z1_", z1[i], "_Z2_", z2[i])]][i])
  }, character(1))
  expect_identical(as.character(revealed), wanted)
})

test_that("potential_outcomes needs an outcome name on the LHS", {
  expect_error(potential_outcomes(~ Z * 2), "LHS of the formula")
})

test_that("reveal_outcomes takes the sep given to potential_outcomes", {
  # 1.0.2 and 2.0 alike always rebuilt the name with "_", so a design written
  # with any other `sep` could not be revealed at all.
  z <- c(0, 1, 0, 1)
  d <- fabricate(
    N = 4, U = rnorm(N),
    potential_outcomes(Y ~ 0.5 * Z + U, sep = "."),
    Z = z,
    Yobs = reveal_outcomes(Y ~ Z, sep = ".")
  )
  expect_true(all(c("Y.Z.0", "Y.Z.1") %in% names(d)))
  expect_identical(d$Yobs, ifelse(z == 1, d$Y.Z.1, d$Y.Z.0))

  # A separator that is not syntactic in a name is kept as written too.
  z1 <- c(0, 1, 0, 1)
  z2 <- c(0, 0, 1, 1)
  d <- fabricate(
    N = 4,
    potential_outcomes(Y ~ Z1 + 10 * Z2,
                       conditions = list(Z1 = 0:1, Z2 = 0:1), sep = "-"),
    Z1 = z1, Z2 = z2,
    Yobs = reveal_outcomes(Y ~ Z1 + Z2, sep = "-")
  )
  expect_true(all(c("Y-Z1-0-Z2-0", "Y-Z1-1-Z2-1") %in% names(d)))
  expect_equal(d$Yobs, z1 + 10 * z2)
})

test_that("a sep that does not match potential_outcomes is named", {
  expect_error(
    fabricate(
      N = 2,
      potential_outcomes(Y ~ Z, sep = "."),
      Z = c(0, 1),
      Yobs = reveal_outcomes(Y ~ Z)
    ),
    "can't find the potential outcome column `Y_Z_0`, `Y_Z_1`.*`sep`"
  )
  expect_error(
    fabricate(
      N = 2,
      potential_outcomes(Y ~ Z),
      Z = c(0, 2),
      Yobs = reveal_outcomes(Y ~ Z)
    ),
    "can't find the potential outcome column `Y_Z_2`"
  )
})

test_that("negative conditions keep their names and reveal", {
  # `as.data.frame()` checked the names, which made `Y_Z_-1` into `Y_Z_.1`,
  # and reveal_outcomes() pasted the name it wanted into code, where `Y_Z_-1`
  # parses as `Y_Z_ - 1` and failed with `object 'Y_Z_' not found`.
  expect_named(
    potential_outcomes(Y ~ Z, conditions = list(Z = c(-1, 0, 1))),
    c("Y_Z_-1", "Y_Z_0", "Y_Z_1")
  )

  z <- c(-1, 1, 0, -1)
  d <- fabricate(
    N = 4,
    potential_outcomes(Y ~ 2 * Z, conditions = list(Z = c(-1, 0, 1))),
    Z = z,
    Yobs = reveal_outcomes(Y ~ Z)
  )
  expect_identical(d[["Y_Z_-1"]], rep(-2, 4))
  expect_identical(d$Yobs, 2 * z)
})

test_that("character conditions with spaces keep their names and reveal", {
  z <- c("b c", "a", "b c")
  d <- fabricate(
    N = 3,
    potential_outcomes(Y ~ paste0("<", Z, ">"),
                       conditions = list(Z = c("a", "b c"))),
    Z = z,
    Yobs = reveal_outcomes(Y ~ Z)
  )
  expect_true(all(c("Y_Z_a", "Y_Z_b c") %in% names(d)))
  expect_identical(d$Yobs, paste0("<", z, ">"))
})

test_that("reveal_outcomes reads columns from a dplyr data mask", {
  d <- fabricate(
    N = 4,
    potential_outcomes(Y ~ 2 * Z, conditions = list(Z = c(-1, 1)))
  )
  z <- c(-1, 1, 1, -1)
  revealed <- dplyr::mutate(d, Z = z, Yobs = reveal_outcomes(Y ~ Z))$Yobs
  expect_identical(revealed, 2 * z)
})

test_that("reveal_outcomes needs bare assignment names on the RHS", {
  d <- fabricate(N = 2, potential_outcomes(Y ~ Z), Z = c(0, 1))
  expect_error(
    fabricate(data = d, Yobs = reveal_outcomes(Y ~ log(Z))),
    "assignment variables named on the right-hand side"
  )
})

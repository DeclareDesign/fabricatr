context("Fabricate")

test_that("Fabricate, basic", {
  basic <- fabricate(N = 2)
  expect_equal(nrow(basic), 2)
  expect_identical(basic$ID, c("1", "2"))
})

test_that("Fabricate, creating variables", {
  y_fix <- fabricate(N = 2, Y = 10)
  expect_true(all(y_fix$Y == 10))
  expect_identical(names(y_fix), c("ID", "Y"))

  new_label <- fabricate(N = 2, Y = 10, ID_label = "test")
  expect_identical(names(new_label), c("test", "Y"))

  add_another_var <- fabricate(N = 2, Y1 = 5, Y2 = rnorm(N))
  expect_equal(ncol(add_another_var), 3)
})

test_that("Moving data after the splat; implicit options for data/N", {
  # Explicit data
  expect_equal(nrow(fabricate(data = sleep)), 20)
  # Explicit N
  expect_equal(nrow(fabricate(N = 20)), 20)
  # Explicit add_level call
  expect_equal(nrow(fabricate(al = add_level(N = 20))), 20)
  # Implicit add_level call
  expect_equal(nrow(fabricate(add_level(N = 20, ID_label="al"))), 20)
  # Implicit data call
  expect_equal(nrow(fabricate(sleep)), 20)
  # Implicit N
  expect_equal(nrow(fabricate(20)), 20)
  # Some named arguments, implicit data
  expect_equal(nrow(fabricate(z = rnorm(N), sleep)), 20)
})

test_that("Reassignment", {
  expect_equal(max(fabricate(
    N = 2, A = 1:2, A = 3
  )$A), 3)
})

test_that("Modify single level", {
  df <- fabricate(N = 2, Y = 10)

  df2 <- fabricate(data = df, Y2 = Y + 1)
  expect_identical(df2$Y, c(10, 10))
  expect_identical(df2$Y2, c(11, 11))
  # TODO: Issue #77 -- # expect_equal(ncol(df2), 4)
})

test_that("Modify single level when import is single column.", {
  msl <- fabricate(
    data = data.frame(existing_data = rnorm(5)),
    Y1 = rnorm(N),
    Y2 = rnorm(N)
  )
  expect_identical(names(msl), c("existing_data", "ID", "Y1", "Y2"))
})

test_that("Modify single level, specify ID to graft.", {
  msl_specify <- fabricate(
    data = data.frame(existing_data = rnorm(5)),
    Y1 = rnorm(N),
    Y2 = rnorm(N),
    ID_label = "IDtest"
  )
  expect_identical(names(msl_specify), c("existing_data", "IDtest", "Y1", "Y2"))
})

test_that("fabricate with a level call", {
  level_call <- fabricate(l1id = add_level(N = 5, doubler = seq(2, N*2, 2)))
  level_call2 <- fabricate(N = 5, doubler = seq(2, N*2, 2), ID_label = "l1id")
  expect_identical(level_call, level_call2)
})

test_that("Fabricate with multiple level calls", {

  fab_nest <- fabricate(
    regions = add_level(N = 5, gdp = rnorm(N)),
    cities = add_level(N = sample(1:5), subways = gdp + 10)
  )
  # Variable creation worked
  expect_equal(fab_nest$subways, fab_nest$gdp + 10)
  # Stochasticity carried forward
  expect_gt(var(table(fab_nest$regions)), 0)
})

test_that("Matrix to DF conversion", {
  # User provides matrix, test conversion.
  m2df <- fabricate(data = matrix(
    rep(c(1, 2, 3), 3),
    byrow = TRUE,
    ncol = 3,
    nrow = 3
  ))
  expect_identical(names(m2df), c("X1", "X2", "X3"))
})

test_that("choose N of a level based on data from higher levels", {
  test_higher_n <- fabricate(
    regions = add_level(N = 3, gdp = 1:3),
    cities = add_level(
      N = gdp * 10 + 1,
      subways = rnorm(N, mean = 5)
    )
  )
  expect_equal(dim(test_higher_n), c(63, 4))
})

test_that("Import data, single level var modification, with/without ID", {
  expect_equal(
    ncol(fabricate(datasets::BOD, dd = demand * 2, ID_label = "Time")),
    3
  )

  expect_equal(
    ncol(fabricate(datasets::BOD, dd = demand * 2)),
    4
  )

  expect_equal(
    ncol(fabricate(datasets::BOD, dd = demand * 2, ID_label = "Jello")),
    4
  )
})


test_that("trigger errors", {
  # User didn't provide a name for a level, and let's make sure that we also
  # didn't interpret the unnamed level as any of the special arguments
  # contextually
  expect_error(fabricate(
    data = NULL,
    N = NULL,
    ID_label = NULL,
    countries = add_level(N = 10),
    add_level(N = 5, population = rnorm(N))
  ))

  # No N in level call
  expect_error(fabricate(
    regions = add_level(),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = 5))
  ))

  # Malformed N in level call
  expect_error(fabricate(
    regions = add_level(N = c(1, 2)),
    cities = add_level(N = sample(1:5), subways = rnorm(N, mean = 5))
  ))

  # Malformed N in nested level call
  expect_error(fabricate(
    regions = add_level(N = 2),
    cities = add_level(N = c(5, 5, 5), subways = rnorm(N, mean = 5))
  ))

  # Character vector N
  expect_error(fabricate(
    regions = add_level(N = 2),
    cities = add_level(N = "N that is a character vector",
                       subways = rnorm(N, mean = 5))
  ))

  # No N, no data
  expect_error(fabricate(test1 = runif(10),
                         test2 = test1 * 3 * runif(10, 1, 2)))

  # Non-integer N:
  expect_error(fabricate(N = 3.5, test1 = runif(3)))

  # Vector N:
  expect_error(fabricate(N = c(3, 4), test1 = runif(3)))
  expect_error(fabricate(N = c(3, 4), test1 = runif(3), ID_label = "my_id"))

  # Non-numeric N
  expect_error(fabricate(N = "hello", test1 = runif(3)))

  # Negative N
  expect_error(fabricate(N = -1, test1 = runif(10)))

  # Scalar as data
  expect_error(fabricate(data = c(5)))
  # Vector as ID_label
  expect_error(fabricate(N = 10,
                         test1 = rnorm(10),
                         test2 = rpois(10, lambda = 2),
                         ID_label = c("invalid", "id")))
  # Matrix as ID_label
  expect_error(fabricate(N = 10,
                         test1 = rnorm(10),
                         test2 = rpois(10, lambda = 2),
                         ID_label = matrix(rep(c(1, 2, 3), 3),
                                           byrow = TRUE, ncol = 3, nrow = 3)))
  # Numeric as ID_label is an error
  expect_error(fabricate(N = 10,
                           test1 = rnorm(10),
                           test2 = rpois(10, lambda = 2),
                           ID_label = 7))
  # Character as ID_label
  fabricate(N = 10,
            test1 = rnorm(10),
            test2 = rpois(10, lambda = 2),
            ID_label = "hello")
  fabricate(N = 10,
            test1 = rnorm(10),
            test2 = rpois(10, lambda = 2),
            ID_label = c("hello"))
  # undefined object as ID_label
  expect_error(fabricate(N = 10,
                         test1 = rnorm(10),
                         test2 = rpois(10, lambda = 2),
                         ID_label = test1))
  expect_error(fabricate(N = 10,
                         test1 = rnorm(10),
                         test2 = rpois(10, lambda = 2),
                         ID_label = test3))

  # Unusual test with implicit data argument
  expect_error(fabricate(N = 10, 1:N))
})

test_that("No name in level", {
  # TODO: Test is potentially broken here.
  expect_error(fabricate(
    add_level(
      N = 5,
      gdp = rnorm(N)
      ),
    add_level(
      N = sample(1:5),
      subways = rnorm(N, mean = gdp)
      )
    ))
})

test_that("Add level interpreted as data import to fabricate", {
  expect_error(fabricate(add_level(
    N = 5,
    gdp = rnorm(N)
  )))
})

test_that("modify_level call when you probably meant add_level", {
  expect_error(fabricate(countries = modify_level(N = 10, new_var = rnorm(N))))
})

test_that("modify_level call where you don't specify which level", {
  expect_error(fabricate(
    countries = add_level(N = 20),
    modify_level(ID_new = as.numeric(ID) * 2)
  ))
})

test_that("modify_level call where you don't specify which level", {
  expect_error(fabricate(a=modify_level(b=1)), "no working data frame to modify")
})

test_that("nest_level call when there was no data to nest", {
  # No import data, nest level
  expect_error(fabricate(countries = nest_level(N = 10, new_var = rnorm(N))))

  # Import data, should be able to nest level
  df_gen <- fabricate(datasets::BOD, units = nest_level(N = 2, dd = demand * 2))
  expect_identical(df_gen$dd, df_gen$demand * 2)
})


test_that("multiple non-nested data frames, again and again", {
  multiple_nnest <- fabricate(
    l1 = add_level(N = 100),
    l2 = add_level(N = 200, nest = FALSE),
    l3 = add_level(N = 100, nest = FALSE),
    l4 = add_level(N = 300, nest = FALSE)
  )
  expect_equal(dim(multiple_nnest), c(300, 1))
})

test_that("ID_label stapling for fabricate calls with no levels", {
  # Create an import data frame
  df <- fabricate(N = 100, d1 = rnorm(N), ID_label = "hello")

  # Import it and verify we don't staple another one if the ID label is
  # already there
  df2 <- fabricate(df, ID_label = "hello", new_var1 = d1 * 2)
  expect_equal(length(colnames(df2)), 3)

  # Import it and verify we don't staple another one if AN ID label is
  # already there and ID_label is not specified
  df3 <- fabricate(df, new_var1 = d1 * 2)
  expect_equal(length(colnames(df3)), 3)

  # Verify we do staple if ID_label is specified and not there
  df4 <- fabricate(df, new_var1 = d1 * 2, ID_label = "newid")
  expect_equal(length(colnames(df4)), 4)

  # Verify we do not staple if nothing changes
  df5 <- fabricate(df)
  expect_equal(length(colnames(df5)), 2)
})

test_that("Multivariate", {
  skip_if_not_installed("MASS")
  set.seed(99999)
  df <- fabricate(N=100, Y=MASS::mvrnorm(N, 0:2, Sigma = matrix(1, 3,3)))
  expect_named(df, c("ID", "Y.1", "Y.2", "Y.3"))
  expect_equal(dim(df), c(100,4))


  df <- fabricate(N=100, Y=setNames(data.frame(MASS::mvrnorm(N, 0:2, Sigma = matrix(1, 3,3))), c("A","B","C")))
  expect_named(df, c("ID", "Y.A", "Y.B", "Y.C"), info = "names borrowed from data.frame")
  expect_equal(dim(df), c(100,4))

  expect_error(fabricate(N=10, Y=MASS::mvrnorm(20, 0:2, Sigma = matrix(1, 3,3)) ), "Nested structures must have `N.` rows")
})

test_that("Error on blank argument when no data or N explicitly passed in",{
  expect_error(fabricate(,sleep), "blank argument")
})


test_that("malformed call where ID is both only column and nonuniqe",{
  df <- fabricate(a=add_level(N=10), a=modify_level(a=as.numeric(a) %/% 2), a=modify_level(b=1))
  expect_equal(dim(df), c(10,2))
})

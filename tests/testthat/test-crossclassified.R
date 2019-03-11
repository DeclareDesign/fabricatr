context("Fabricate Cross Classified")

test_that("Panel data, well-formed", {
  set.seed(19861108)

  # Well formed
  panel <- fabricate(
    year = add_level(N = 20, year_shock = runif(N, 1, 10)),
    country = add_level(N = 20, country_shock = runif(N, 1, 10), nest = FALSE),
    obs = cross_levels(
      by = join(year, country),
      GDP_it = country_shock + year_shock
    )
  )
  expect_equal(nrow(panel), 20 * 20)
  expect_equal(
    panel[1, ]$GDP_it,
    panel[1, ]$country_shock + panel[1, ]$year_shock
  )
})

test_that("Panel data, errors.", {
  set.seed(19861108)

  # Error: Specified correlation with a panel
  expect_error(fabricate(
    year = add_level(N = 20, year_shock = runif(N, 1, 10)),
    country = add_level(N = 20, country_shock = runif(N, 1, 10), nest = FALSE),
    obs = cross_levels(
      by = join(year, country, rho = 0.5),
      GDP_it = country_shock + year_shock
    )
  ))

  # Error: Only one join
  expect_error(fabricate(
    year = add_level(N = 20, year_shock = runif(N, 1, 10)),
    country = add_level(N = 20, country_shock = runif(N, 1, 10), nest = FALSE),
    obs = cross_levels(
      by = join(year),
      GDP_it = country_shock + year_shock
    )
  ))
})

test_that("Cross-classified data", {
  set.seed(19861108)

  # Example draw setup
  students <- fabricate(
    primary_schools = add_level(
      N = 100,
      ps_quality = runif(n = N, 1, 100),
      ps_hasband = draw_binary(0.5, N = N),
      ps_testscores = ps_quality * 5 + rnorm(N, 30, 5)
    ),
    secondary_schools = add_level(
      N = 50,
      ss_quality = runif(n = N, 1, 100),
      ss_hascomputers = draw_binary(ss_quality / 100, N = N),
      ss_testscores = ss_quality * 5 + rnorm(N, 30, 5),
      nest = FALSE
    ),
    students = link_levels(
      N = 1000,
      by = join(ps_quality, ss_quality, rho = 0.5),
      student_score = ps_testscores * 5 + ss_testscores * 10 + rnorm(N, 10, 5),
      student_score_2 = student_score * 2,
      extracurricular = ps_hasband + ss_hascomputers
    )
  )

  # Within a reasonable "tolerance"
  expect_gte(cor(students$ps_quality, students$ss_quality), 0.3)
  expect_lte(cor(students$ps_quality, students$ss_quality), 0.7)
})

test_that("Cross-classified data, uncorrelated", {
  set.seed(19861108)

  # Uncorrelated
  students_uncorr <- fabricate(
    primary_schools = add_level(
      N = 100,
      ps_quality = runif(n = N, 1, 100),
      ps_hasband = draw_binary(0.5, N = N),
      ps_testscores = ps_quality * 5 + rnorm(N, 30, 5)
    ),
    secondary_schools = add_level(
      N = 50,
      ss_quality = runif(n = N, 1, 100),
      ss_hascomputers = draw_binary(ss_quality / 100, N = N),
      ss_testscores = ss_quality * 5 + rnorm(N, 30, 5),
      nest = FALSE
    ),
    students = link_levels(
      N = 1000,
      by = join(ps_quality, ss_quality, rho = 0),
      student_score = ps_testscores * 5 + ss_testscores * 10 + rnorm(N, 10, 5),
      student_score_2 = student_score * 2,
      extracurricular = ps_hasband + ss_hascomputers
    )
  )

  # Again, within tolerance
  expect_gte(cor(students_uncorr$ps_quality, students_uncorr$ss_quality), -0.15)
  expect_lte(cor(students_uncorr$ps_quality, students_uncorr$ss_quality), 0.15)
})

test_that("Cross-classified, sigma in lieu of rho.", {
  # Specifying sigma in lieu of rho
  test_next <- fabricate(
    l1 = add_level(N = 50, j1 = rnorm(N)),
    l2 = add_level(N = 50, j2 = rnorm(N), nest = FALSE),
    joined = link_levels(
      N = 200,
      by = join(j1, j2, sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2))
    )
  )

  expect_gte(cor(test_next$j1, test_next$j2), 0.3)
  expect_lte(cor(test_next$j1, test_next$j2), 0.7)
})

test_that("Cross-classified, code path without mvnfast", {
  set.seed(19861108)

  # Need to directly call joint_draw_ecdf because we don't let users voluntarily
  # override the use_f argument
  dl <- list(
    j1 = rnorm(100),
    j2 = rnorm(500)
  )
  result <- fabricatr:::joint_draw_ecdf(dl, N = 100, rho = 0.3, use_f = FALSE)
  data <- cbind(
    dl$j1[result[[1]]],
    dl$j2[result[[2]]]
  )
  expect_gte(cor(data[, 1], data[, 2]), 0.1)
  expect_lte(cor(data[, 1], data[, 2]), 0.5)
})

test_that("Deliberate failures in join_dfs", {
  df1 <- fabricate(N = 100, j1 = rnorm(100))
  df2 <- fabricate(N = 100, j2 = rnorm(100))
  df3 <- fabricate(N = 100, j3 = rnorm(100))

  expect_error(fabricatr:::join_dfs(df1, c("j1"), N = 100, rho = 0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1"),
                                    N = 100, rho = 0.5))
  expect_error(fabricatr:::join_dfs(list(df1), c("j1"), N = 100, rho = 0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1", "j2"),
                                    N = -1, rho = 0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1"),
                                    N = c(3, 10), rho = 0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2, df3), c("j1", "j2", "j3"),
                                    N = 100, rho = -0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1", "j2"),
                                    N = 100, rho = c(0.5, 0.3)))

  expect_error(fabricatr:::join_dfs(
    list(df1, df2), c("j1", "j2"),
    N = 100,
    sigma = matrix(
      c(1, 0.3, 0.3, 0.3, 1, 0.3, 0.3, 0.3, 1),
      ncol = 3
    )
  ))
})

test_that("Deliberate failures in link_levels", {
  expect_error(
    fabricate(
      l1 = add_level(N = 50, j1 = rnorm(N)),
      l2 = add_level(N = 50, j2 = rnorm(N), nest = FALSE),
      joined = link_levels(
        N = 200,
        by = join(
          j1,
          j_error,
          sigma = matrix(
            c(1, 0.5, 0.5, 1),
            ncol = 2
          )
        )
      )
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50, j1 = rnorm(N)),
      l2 = add_level(N = 50,
                     j_var = rnorm(N),
                     j1 = runif(N, 1, 3),
                     nest = FALSE),
      joined = link_levels(
        N = 200,
        by = join(j1, j_var, sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2))
      )
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50, j1 = rnorm(N)),
      l2 = add_level(N = 50, j2 = rnorm(N), nest = FALSE),
      joined = link_levels(N = 200)
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50),
      joined = link_levels(N = 200)
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50, v1 = rnorm(N), v2 = rnorm(N), v3 = rnorm(N)),
      l2 = add_level(N = 30, v4 = rnorm(N), nest = FALSE),
      joined = link_levels(N = 100, by = join(v1, v2))
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50, v1 = rnorm(N), v2 = rnorm(N), v3 = rnorm(N)),
      l2 = add_level(N = 30, v4 = rnorm(N), nest = FALSE),
      joined = link_levels(N = 100, by = join(v1, v4, v1))
    )
  )
})

test_that("Cross-classified with double import", {
  set.seed(19861108)

  primary_schools <- fabricate(
    N = 100,
    ps_quality = runif(n = N, 1, 100),
    ps_hasband = draw_binary(0.5, N = N),
    ps_testscores = ps_quality * 5 + rnorm(N, 30, 5),
    ID_label = "primary_schools"
  )
  secondary_schools <- fabricate(
    N = 50,
    ss_quality = runif(n = N, 1, 100),
    ss_hascomputers = draw_binary(ss_quality / 100, N = N),
    ss_testscores = ss_quality * 5 + rnorm(N, 30, 5),
    ID_label = "secondary_schools"
  )

  students <- fabricate(
    list(primary_schools, secondary_schools),
    students = link_levels(
      N = 1000,
      by = join(ps_quality, ss_quality, rho = 0.5),
      student_score = ps_testscores * 5 + ss_testscores * 10 + rnorm(N, 10, 5),
      student_score_2 = student_score * 2,
      extracurricular = ps_hasband + ss_hascomputers
    )
  )

  # Within a reasonable "tolerance"
  expect_equal(nrow(students), 1000)
  expect_gte(cor(students$ps_quality, students$ss_quality), 0.3)
  expect_lte(cor(students$ps_quality, students$ss_quality), 0.7)
})

test_that("Cross_levels wrapper.", {
  expect_error(fabricate(
    l1 = add_level(N = 10),
    l2 = add_level(N = 10, nest = FALSE),
    l3 = cross_levels(N = 10, by = join(l1, l2))
  ))

  expect_error(fabricate(
    l1 = add_level(N = 10),
    l2 = add_level(N = 10, nest = FALSE),
    l3 = cross_levels(by = join(l1, l2, rho = 0.2))
  ))

  expect_error(fabricate(
    l1 = add_level(N = 10),
    l2 = add_level(N = 10, nest = FALSE),
    l3 = cross_levels(N = 10, by = join(
      l1, l2,
      sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)
    ))
  ))

  z <- fabricate(
    l1 = add_level(N = 10),
    l2 = add_level(N = 10, nest = FALSE),
    l3 = cross_levels(by = join(l1, l2))
  )
  expect_equal(nrow(z), 100)
  expect_equal(ncol(z), 3)
})

test_that("Malformed sigma in link_levels", {
  set.seed(19861108)

  primary_schools <- fabricate(
    N = 100,
    ps_quality = runif(n = N, 1, 100),
    ID_label = "primary_schools"
  )

  secondary_schools <- fabricate(
    N = 50,
    ss_quality = runif(n = N, 1, 100),
    ID_label = "secondary_schools"
  )

  universities <- fabricate(
    N = 70,
    u_quality = runif(n = N, 1, 100),
    ID_label = "universities"
  )

  non_square_matrix <- matrix(c(1, 0.5, 0.8,
                               0.5, 1, 0.3), byrow=TRUE, ncol=3, nrow=2)
  out_of_range_matrix <- matrix(c(1, 2,
                            2, 1), byrow=TRUE, ncol=2, nrow=2)
  asymmetric_matrix <- matrix(c(1, 0.8,
                            -0.5, 1), byrow=TRUE, ncol=2, nrow=2)
  non_psd_matrix <- matrix(c(1, -0.8, -0.5,
                            -0.8, 1, -0.5,
                            -0.5, -0.5, 1), byrow=TRUE, ncol=3, nrow=3)

  expect_error(fabricate(list(primary_schools, secondary_schools),
                         students = link_levels(
                           N = 1000,
                           by = join(ps_quality, ss_quality,
                                     sigma=non_square_matrix)
                         )))

  expect_error(fabricate(list(primary_schools, secondary_schools),
                         students = link_levels(
                           N = 1000,
                           by = join(ps_quality, ss_quality,
                                     sigma=out_of_range_matrix)
                         )))

  expect_error(fabricate(list(primary_schools, secondary_schools),
                         students = link_levels(
                           N = 1000,
                           by = join(ps_quality, ss_quality,
                                     sigma=asymmetric_matrix)
                         )))

  expect_error(fabricate(list(primary_schools, secondary_schools, universities),
                         students = link_levels(
                           N = 1000,
                           by = join(ps_quality, ss_quality, u_quality,
                                     sigma=non_psd_matrix)
                         )))
})

test_that("crossing from common parent", {
  set.seed(19861108)

  # Well formed
  panel <- fabricate(
    a = add_level(N = 5),
    b = add_level(N = 4),
    a = modify_level(a=a),
    c = add_level(N=3),
    obs = cross_levels(
      by = join(b,c)
    )
  )
  expect_equal(nrow(panel), 15*20)
  expect_named(
    panel,
    c("a.x", "b", "a.y", "c", "obs")
  )
})


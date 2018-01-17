context("Fabricate")

test_that("Cross-classified data", {
  set.seed(19861108)

  # Example draw setup
  students = fabricate(
    primary_schools = add_level(N = 100,
                                ps_quality = runif(n=N, 1, 100),
                                ps_hasband = draw_binary(0.5, N=N),
                                ps_testscores = ps_quality * 5 + rnorm(N, 30, 5)),
    secondary_schools = add_level(N = 50,
                                  ss_quality = runif(n=N, 1, 100),
                                  ss_hascomputers = draw_binary(ss_quality/100, N=N),
                                  ss_testscores = ss_quality * 5 + rnorm(N, 30, 5),
                                  nest = FALSE),
    students = cross_level(N = 1000,
                           by = join(ps_quality, ss_quality, rho=0.5),
                           student_score = ps_testscores * 5 + ss_testscores * 10 + rnorm(N, 10, 5),
                           student_score_2 = student_score * 2,
                           extracurricular = ps_hasband + ss_hascomputers
    )
  )

  # Within a reasonable "tolerance"
  expect_gte(cor(students$ps_quality, students$ss_quality), 0.3)
  expect_lte(cor(students$ps_quality, students$ss_quality), 0.7)

  # Uncorrelated
  students_uncorr = fabricate(
    primary_schools = add_level(N = 100,
                                ps_quality = runif(n=N, 1, 100),
                                ps_hasband = draw_binary(0.5, N=N),
                                ps_testscores = ps_quality * 5 + rnorm(N, 30, 5)),
    secondary_schools = add_level(N = 50,
                                  ss_quality = runif(n=N, 1, 100),
                                  ss_hascomputers = draw_binary(ss_quality/100, N=N),
                                  ss_testscores = ss_quality * 5 + rnorm(N, 30, 5),
                                  nest = FALSE),
    students = cross_level(N = 1000,
                           by = join(ps_quality, ss_quality, rho=0),
                           student_score = ps_testscores * 5 + ss_testscores * 10 + rnorm(N, 10, 5),
                           student_score_2 = student_score * 2,
                           extracurricular = ps_hasband + ss_hascomputers
    )
  )

  # Again, within tolerance
  expect_gte(cor(students_uncorr$ps_quality, students_uncorr$ss_quality), -0.15)
  expect_lte(cor(students_uncorr$ps_quality, students_uncorr$ss_quality), 0.15)


  # Specifying sigma in lieu of rho
  test_next = fabricate(
    l1 = add_level(N = 50, j1 = rnorm(N)),
    l2 = add_level(N = 50, j2 = rnorm(N), nest=FALSE),
    joined = cross_level(N = 200,
                         by = join(j1, j2, sigma=matrix(c(1, 0.5, 0.5, 1), ncol=2)))
  )

  expect_gte(cor(test_next$j1, test_next$j2), 0.3)
  expect_lte(cor(test_next$j1, test_next$j2), 0.7)
})

test_that("Code path without mvnfast", {
  set.seed(19861108)

  # Need to directly call joint_draw_ecdf because we don't let users voluntarily
  # override the use_f argument
  dl = list(j1 = rnorm(100),
            j2 = rnorm(500))
  result = fabricatr:::joint_draw_ecdf(dl, N = 100, rho = 0.3, use_f = FALSE)
  data = cbind(dl$j1[result[[1]]],
               dl$j2[result[[2]]])
  expect_gte(cor(data[, 1], data[, 2]), 0.1)
  expect_lte(cor(data[, 1], data[, 2]), 0.5)
})

test_that("Deliberate failures in join_dfs", {
  df1 = fabricate(N=100, j1 = rnorm(100))
  df2 = fabricate(N=100, j2 = rnorm(100))
  df3 = fabricate(N=100, j3 = rnorm(100))

  expect_error(fabricatr:::join_dfs(df1, c("j1"), N=100, rho=0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1"), N=100, rho=0.5))
  expect_error(fabricatr:::join_dfs(list(df1), c("j1"), N=100, rho=0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1", "j2"), N=-1, rho=0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1"), N=c(3, 10), rho=0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2, df3), c("j1", "j2", "j3"), N=100, rho=-0.5))
  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1", "j2"), N=100, rho=c(0.5, 0.3)))

  expect_error(fabricatr:::join_dfs(list(df1, df2), c("j1", "j2"),
                                   N=100,
                                   sigma=matrix(c(1, 0.3, 0.3, 0.3, 1, 0.3, 0.3, 0.3, 1),
                                                ncol = 3
                                                )))
})

test_that("Deliberate failures in cross_level", {
  expect_error(
    fabricate(
      l1 = add_level(N = 50, j1 = rnorm(N)),
      l2 = add_level(N = 50, j2 = rnorm(N), nest=FALSE),
      joined = cross_level(N = 200,
                           by = join(j1,
                                     j_error,
                                     sigma=matrix(c(1, 0.5, 0.5, 1),
                                                  ncol=2)
                                     )
                           )
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50, j1 = rnorm(N)),
      l2 = add_level(N = 50, j_var = rnorm(N), j1 = runif(N, 1, 3), nest=FALSE),
      joined = cross_level(N = 200,
                           by = join(j1, j_var, sigma=matrix(c(1, 0.5, 0.5, 1), ncol=2)))
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50, j1 = rnorm(N)),
      l2 = add_level(N = 50, j2 = rnorm(N), nest=FALSE),
      joined = cross_level(N = 200)
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50),
      joined = cross_level(N = 200)
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50, v1 = rnorm(N), v2 = rnorm(N), v3 = rnorm(N)),
      l2 = add_level(N = 30, v4 = rnorm(N), nest=FALSE),
      joined = cross_level(N = 100, by=join(v1, v2))
    )
  )

  expect_error(
    fabricate(
      l1 = add_level(N = 50, v1 = rnorm(N), v2 = rnorm(N), v3 = rnorm(N)),
      l2 = add_level(N = 30, v4 = rnorm(N), nest=FALSE),
      joined = cross_level(N = 100, by=join(v1, v4, v1))
    )
  )
})

test_that("Cross-classified with double import", {
  set.seed(19861108)

  primary_schools = fabricate(N = 100,
                              ps_quality = runif(n=N, 1, 100),
                              ps_hasband = draw_binary(0.5, N=N),
                              ps_testscores = ps_quality * 5 + rnorm(N, 30, 5),
                              ID_label = "primary_schools")
  secondary_schools = fabricate(N = 50,
                                ss_quality = runif(n=N, 1, 100),
                                ss_hascomputers = draw_binary(ss_quality/100, N=N),
                                ss_testscores = ss_quality * 5 + rnorm(N, 30, 5),
                                ID_label = "secondary_schools")

  students = fabricate(
    list(primary_schools, secondary_schools),
    students = cross_level(N = 1000,
                           by = join(ps_quality, ss_quality, rho=0.5),
                           student_score = ps_testscores * 5 + ss_testscores * 10 + rnorm(N, 10, 5),
                           student_score_2 = student_score * 2,
                           extracurricular = ps_hasband + ss_hascomputers
    )
  )

  # Within a reasonable "tolerance"
  expect_gte(cor(students$ps_quality, students$ss_quality), 0.3)
  expect_lte(cor(students$ps_quality, students$ss_quality), 0.7)
})

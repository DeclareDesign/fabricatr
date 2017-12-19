context("Fabricate")

test_that("Cross-classified data", {
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
  expect_gte(cor(students_uncorr$ps_quality, students_uncorr$ss_quality), -0.1)
  expect_lte(cor(students_uncorr$ps_quality, students_uncorr$ss_quality), 0.1)


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

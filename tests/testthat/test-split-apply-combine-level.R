context("sac_level()")

test_that("base case",{

  out <- fabricate(sleep, sleep=sac_level(by="group", ybar=mean(extra)))

  # ybar constant within group
  expect_true(all(tapply(out$ybar, out$group, function(x) all(x == x[1]))))


})



test_that("modify_level works to create variables at higher levels that are functions of variables at lower levels", {

  df <- fabricate(block = add_level(N = 2),
                  i     = add_level(N = 2, u = 5))

  df <- fabricate(data  = df,
                  block = modify_level(mean_u = mean(u)))

  expect_equal(df$mean_u, rep(5, 4))

})

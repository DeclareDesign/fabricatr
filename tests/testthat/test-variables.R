context("Variable functions")

test_that("Variable functions",{

level(ID_label = "bob", N = 10, Y1 = rnorm(N),  Y2 = binary_logit(Y1))

level(ID_label = "bob", N = 10, Y1 = rnorm(N),  Y2 = binomial_count(Y1, k = 3))


binary_logit(runif(100))
binomial_count(runif(100), 4)

})

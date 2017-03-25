library(magrittr)
library(DDfabricate)


#debugonce(fabricate_data_single_level_)
DDfabricate:::fabricate_data_single_level_(N = 10, "Y1 = rnorm(N)", "Y2 = rnorm(N)")
#debugonce(fabricate_data_)
fabricate_data_(N = 10, "Y1 = rnorm(N)", "Y2 = rnorm(N)")
#debugonce(fabricate_data)
fabricate_data(N = 10, Y1 = rnorm(N), Y2 = rnorm(N))

fabricate_data_(N = 5, "var1 = rnorm(N)")


##debugonce(level_)
level_("bob", N = 10, "Y1 = rnorm(N)", "Y2 = rnorm(N)")

##debugonce(level)
level(bob, N = 10, Y1 = rnorm(N), Y2 = rnorm(N))

fabricate_data(level(N = 5, gdp = rnorm(N), level_name = "regions"),
               level(N = 12, subways = rnorm(N, mean = gdp), level_name = "cities"))

fabricate_data(level(N = 5, gdp = rnorm(N), level_name = "regions"),
               level(N = 1:5, subways = rnorm(N, mean = gdp), level_name = "cities"))

fabricate_data(level(N = 5, gdp = rnorm(N), level_name = "regions"),
               level(N = sample(1:5), subways = rnorm(N, mean = gdp), level_name = "cities"))


library(DDfabricate)
library(magrittr)

df <- fabricate_data_single_level(N = 2, Y = 10)
df_2 <- fabricate_data_single_level(data = df, Y2 = Y + 1,ID_label = "BOB")

level(ID_label = "test", N = 3, Y = 10)
level(ID_label = "test", N = 4, Y = 10, data = df_2)


# we expect an error here, but need a better explaination (no unnamed arguments)
#fabricate_data_single_level(N = 10, rnorm(N), Y2 = rnorm(N))


level(ID_label = "bob", N = 10, Y1 = rnorm(N), Y2 = rnorm(N))

# can it be called?  YES WE CAN
wrapitup <- function(N){
  level(ID_label = "bob", N = N, Y1 = rnorm(N), Y2 = rnorm(N))
}

wrapitup(12)


fabricate_data(N = 2, Y = 10, ID_label = "test")
fabricate_data(N = 2, Y = 10)
fabricate_data(N = 2, Y1 = rnorm(N), Y2 = rnorm(N))

fabricate_data(level(N = 5, gdp = rnorm(N), ID_label = "regions"),
               level(N = sample(1:5), subways = rnorm(N, mean = gdp), ID_label = "cities"))

fabricate_data(regions = level(N = 5, gdp = rnorm(N)),
               cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

declare_population(N = 2, Y = 10, ID_label = "test")()
declare_population(N = 2, Y = 10)()



rm(list=ls())

library(lazyeval)

my_custom_pop <- function(N){
  data.frame(income = runif(N))
}


declare_population <- function(
  population_function = fabricate_data,
  ...){
  func <-
    function(){
      ##evaluated_dots <- lapply(dots_capture(...), f_eval)
      f_eval(~ uq(population_function)(uqs(villages)))
    }
}

##debugonce(declare_population)
##declare_population(villages = rnorm(N))()


##declare_population(level(N = 5, gdp = rnorm(N), ID_label = "regions"),
##                   level(N = sample(1:5), subways = rnorm(N, mean = gdp), ID_label = "cities"))()

declare_population(regions = level(N = 5, gdp = rnorm(N)),
                   cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))()


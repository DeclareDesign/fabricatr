## ------------------------------------------------------------------------
library(fabricatr)
my_data <- fabricate(N = 5, Y = runif(N), Y2 = Y*5)
my_data

## ------------------------------------------------------------------------
my_data <-
  fabricate(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = 3, income = round(elevation * rnorm(n = N, mean = 5)))
  )
my_data

## ------------------------------------------------------------------------
fabricate(N = 3, normal_var = rnorm(N))

## ------------------------------------------------------------------------
fabricate(N = 3, p = c(0, .5, 1), binary = draw_discrete(p))

## ------------------------------------------------------------------------
fabricate(N = 3, p = c(0, .5, 1), binary = draw_discrete(p, type = "bernoulli"))

## ------------------------------------------------------------------------
fabricate(N = 3, x = 10*rnorm(N), binary = draw_discrete(x, type = "bernoulli", link = "probit"))

## ------------------------------------------------------------------------
fabricate(N = 3, p = c(0, .5, 1), binomial = draw_discrete(p, type = "binomial", k = 10))

## ------------------------------------------------------------------------
fabricate(N = 3, x = 10*rnorm(N), binomial = draw_discrete(x, type = "binomial", k = 10, link = "logit"))

## ------------------------------------------------------------------------
set.seed(1)
fabricate(N = 3, x = 5*rnorm(N), ordered = draw_discrete(x, type = "ordered", breaks = c(-Inf, -1, 1, Inf)))

## ------------------------------------------------------------------------
set.seed(1)
fabricate(N = 3, x = 5*rnorm(N), ordered = draw_discrete(x, type = "ordered", breaks = c(-Inf, -1, 1, Inf), link = "probit"))

## ------------------------------------------------------------------------
fabricate(N = 3, x = c(0,5,100), count = draw_discrete(x, type = "count"))

## ------------------------------------------------------------------------
fabricate(N = 6, p1 = runif(N), p2 = runif(N), p3 = runif(N),
          cat = draw_discrete(cbind(p1, p2, p3), type = "categorical"))

## ------------------------------------------------------------------------
baseline_survey <- fabricate(N = 5, Y_pre = rnorm(N))

my_endline <- fabricate(data = baseline_survey, 
                             Y_post = Y_pre + rnorm(N))
my_endline

## ------------------------------------------------------------------------
bootstrapped_data <- resample_data(baseline_survey, N = 10)
bootstrapped_data

## ------------------------------------------------------------------------
my_data <-
  fabricate(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = 3, income = round(elevation * rnorm(n = N, mean = 5)))
  )

my_data_2 <- resample_data(my_data, N = c(3, 5), ID_labels = c("cities", "citizens"))
my_data_2

## ------------------------------------------------------------------------
fabricate(
    cities = level(N = 2),
    citizens = level(
      N = 1:2, income = rnorm(N), income_mean_city = ave(income, cities))
  ) 

## ------------------------------------------------------------------------
fabricate(
    cities = level(N = 2),
    citizens = level(
      N = 1:2, income = rnorm(N), income_mean_city = ave(income, cities))
  ) 

## ------------------------------------------------------------------------
my_data <-
  fabricate(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = c(2, 4), income = round(elevation * rnorm(n = N, mean = 5)))
  )
my_data

## ------------------------------------------------------------------------
my_data <-
  fabricate(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = sample(1:6, size = 2, replace = TRUE), income = round(elevation * rnorm(n = N, mean = 5)))
  )
my_data

## ------------------------------------------------------------------------

my_baseline_data <-
  fabricate(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = 3, income = round(elevation * rnorm(n = N, mean = 5)))
  )

# add new variables at each level
my_data <- 
  fabricate(data = my_baseline_data,
                 cities = level(density = elevation / 2),
                 citizens = level(wealth = income - 100))

my_data


## ---- message=FALSE------------------------------------------------------
library(dplyr)

# letting higher levels depend on lower levels

my_data <- 
fabricate(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = c(2, 3), income = round(elevation * rnorm(n = N, mean = 5)))
  ) %>%
  group_by(cities) %>%
  mutate(pop = n())

my_data

my_data <- 
data_frame(Y = sample(1:10, 2)) %>%
  fabricate(lower_level = level(N = 3, Y2 = Y + rnorm(N)))
my_data


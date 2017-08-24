## ------------------------------------------------------------------------
library(fabricatr)
my_data <- fabricate_data(N = 5, Y = runif(N), Y2 = Y*5)
my_data

## ------------------------------------------------------------------------
my_data <-
  fabricate_data(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = 3, income = round(elevation * rnorm(n = N, mean = 5)))
  )
my_data

## ------------------------------------------------------------------------
baseline_survey <- fabricate_data(N = 5, Y_pre = rnorm(N))

my_endline <- fabricate_data(data = baseline_survey, 
                             Y_post = Y_pre + rnorm(N))
my_endline

## ------------------------------------------------------------------------
bootsrapped_data <- resample_data(baseline_survey, N = 10)
bootsrapped_data

## ------------------------------------------------------------------------
my_data <-
  fabricate_data(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = 3, income = round(elevation * rnorm(n = N, mean = 5)))
  )

my_data_2 <- resample_data(my_data, N = c(3, 5), ID_labels = c("cities", "citizens"))
my_data_2

## ------------------------------------------------------------------------
my_data <-
  fabricate_data(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = c(2, 4), income = round(elevation * rnorm(n = N, mean = 5)))
  )
my_data

## ------------------------------------------------------------------------
my_data <-
  fabricate_data(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = sample(1:6, size = 2, replace = TRUE), income = round(elevation * rnorm(n = N, mean = 5)))
  )
my_data

## ------------------------------------------------------------------------

my_baseline_data <-
  fabricate_data(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = 3, income = round(elevation * rnorm(n = N, mean = 5)))
  )

# add new variables at each level
my_data <- 
  fabricate_data(data = my_baseline_data,
                 cities = level(density = elevation / 2),
                 citizens = level(wealth = income - 100))

my_data


## ---- message=FALSE------------------------------------------------------
library(dplyr)

# letting higher levels depend on lower levels

my_data <- 
fabricate_data(
    cities = level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = level(N = c(2, 3), income = round(elevation * rnorm(n = N, mean = 5)))
  ) %>%
  group_by(cities) %>%
  mutate(pop = n())

my_data

my_data <- 
data_frame(Y = sample(1:10, 2)) %>%
  fabricate_data(lower_level = level(N = 3, Y2 = Y + rnorm(N)))
my_data




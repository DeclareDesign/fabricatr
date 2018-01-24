## ---- echo=FALSE---------------------------------------------------------
options(digits=2)
set.seed(19861108)
library(fabricatr)

## ----echo=TRUE, results="hide"-------------------------------------------
variable_data <-
  fabricate(
    cities = add_level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = add_level(N = c(2, 4), age = runif(N, 18, 70))
  )
variable_data

## ----echo=FALSE----------------------------------------------------------
knitr::kable(variable_data)

## ----echo=TRUE, results="hide"-------------------------------------------
my_data <-
  fabricate(
    cities = add_level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = add_level(N = sample(1:6, size = 2, replace = TRUE), age = runif(N, 18, 70))
  )
my_data

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_data)

## ----echo=TRUE, results="hide"-------------------------------------------
variable_n <- fabricate(
  cities = add_level(N = 5, population = runif(N, 10, 200)),
  citizens = add_level(N = round(population * 0.3))
)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(variable_n))

## ---- message=FALSE, echo=TRUE, results="hide"---------------------------
library(dplyr)

my_data <-
  fabricate(
    cities = add_level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = add_level(N = c(2, 3), age = runif(N, 18, 70))
  ) %>%
  group_by(cities) %>%
  mutate(pop = n())

my_data

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_data)

## ----echo=TRUE, results="hide"-------------------------------------------
my_data <-
  data_frame(Y = sample(1:10, 2)) %>%
  fabricate(lower_level = add_level(N = 3, Y2 = Y + rnorm(N)))
my_data

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_data)


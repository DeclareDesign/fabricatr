## ---- echo=FALSE---------------------------------------------------------
options(digits=2)
set.seed(19861108)
library(fabricatr)

## ----echo=TRUE, results="hide"-------------------------------------------
survey_data <- fabricate(N = 10, voted_republican = draw_binary(prob = 0.5, N = N))

survey_data_new <- resample_data(survey_data)
head(survey_data_new)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(survey_data_new))

## ----echo=TRUE, results="hide"-------------------------------------------
large_survey_data <- resample_data(survey_data, N = 100)
nrow(large_survey_data)

## ----echo=TRUE, results="hide"-------------------------------------------
my_data <-
  fabricate(
    cities = add_level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = add_level(N = 3, age = runif(N, 18, 70))
  )

my_data_2 <- resample_data(my_data, N = c(3, 5), ID_labels = c("cities", "citizens"))
head(my_data_2)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(my_data_2))

## ----echo=TRUE, results="hide"-------------------------------------------
my_data <-
  fabricate(
    cities = add_level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = add_level(N = 3, age = runif(N, 18, 70))
  )

my_data_2 <- resample_data(my_data, N = c(cities = 3, citizens = 5))
head(my_data_2)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(my_data_2))

## ----echo=TRUE, results="hide"-------------------------------------------
my_data <-
  fabricate(
    cities = add_level(N = 2, elevation = runif(n = N, min = 1000, max = 2000)),
    citizens = add_level(N = 3, age = runif(N, 18, 70))
  )

my_data_3 <- resample_data(my_data, N = c(ALL, 1), ID_labels = c("cities", "citizens"))
head(my_data_3)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(my_data_3))


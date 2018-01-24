## ---- echo=FALSE---------------------------------------------------------
options(digits = 2, scipen = 8)
knitr::knit_hooks$set(inline = function(x) {
  prettyNum(x, decimal.mark = ".", big.mark = ",")
})
set.seed(19861108)
library(fabricatr)

## ----echo=TRUE, results="hide"-------------------------------------------
library(fabricatr)
my_data <- fabricate(N = 5, Y = runif(N), Y2 = Y * 5)
my_data

## ----echo=FALSE----------------------------------------------------------
knitr::kable(my_data)

## ----echo=TRUE, results="hide"-------------------------------------------
simulated_quake_data <- fabricate(
  data = quakes,
  fatalities = round(pmax(0, rnorm(N, mean = mag)) * 100),
  insurance_cost = fatalities * runif(N, 1000000, 2000000)
)
head(simulated_quake_data)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(simulated_quake_data[, c(1, 2, 3, 4, 5, 7, 8)]), format.args = list(big.mark = ","))

## ----echo=TRUE, results="hide"-------------------------------------------
country_data <-
  fabricate(
    countries = add_level(
      N = 5,
      gdp_per_capita = runif(N, min = 10000, max = 50000),
      life_expectancy = 50 + runif(N, 10, 20) + ((gdp_per_capita > 30000) * 10)
    ),
    provinces = add_level(
      N = 10,
      natural_resources = draw_binary(prob = 0.3, N = N),
      manufacturing = draw_binary(prob = 0.7, N = N)
    )
  )
head(country_data)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(country_data), format.args=list(big.mark = ","))

## ----echo=TRUE, results="hide"-------------------------------------------
citizen_data <-
  fabricate(
    data = country_data,
    citizens = add_level(
      N = 10,
      salary = rnorm(
        N,
        mean = gdp_per_capita + natural_resources * 5000 + manufacturing * 5000,
        sd = 10000
      )
    )
  )
head(citizen_data)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(citizen_data), format.args=list(big.mark = ","))

## ----echo=TRUE, results="hide"-------------------------------------------
new_country_data <-
  fabricate(
    data = country_data,
    countries = modify_level(average_temperature = runif(N, 30, 80))
  )

head(new_country_data)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(head(new_country_data), format.args=list(big.mark = ","))

## ----echo=TRUE, results="hide"-------------------------------------------
new_citizen_data <-
  fabricate(
    data = citizen_data,
    countries = modify_level(average_temperature = runif(N, 30, 80)),
    provinces = modify_level(
      conflict_zone = draw_binary(N, prob = 0.2 + natural_resources * 0.3),
      infant_mortality = runif(N, 0, 10) + conflict_zone * 10 +
        (average_temperature > 70) * 10
    ),
    citizens = modify_level(
      college_degree = draw_binary(N, prob = 0.4 - (0.3 * conflict_zone))
    )
  )

## ----echo=TRUE, results="hide"-------------------------------------------
ave_example <- fabricate(
  cities = add_level(N = 2),
  citizens = add_level(
    N = 1:2, income = rnorm(N),
    income_mean_city = ave(income, cities)
  )
)
ave_example

## ----echo=FALSE----------------------------------------------------------
knitr::kable(ave_example)


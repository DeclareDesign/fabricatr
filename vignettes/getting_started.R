## ---- echo=FALSE---------------------------------------------------------
options(digits=2)
set.seed(19861108)
library(fabricatr)

## ------------------------------------------------------------------------
library(fabricatr)

voters <- fabricate(
  N = 1000,
  group_id = rep(1:10, 100),
  ideology = draw_normal_icc(mean = 0, N = N, clusters = group_id, ICC = 0.7),
  ideological_label = draw_ordered(
    x = ideology,
    break_labels = c(
      "Very Conservative", "Conservative",
      "Liberal", "Very Liberal"
    )
  ),
  income = exp(rlnorm(n = N, meanlog = 2.4 - (ideology * 0.1), sdlog = 0.12)),
  Q1_immigration = draw_likert(x = ideology, type = 7),
  Q2_defence = draw_likert(x = ideology + 0.5, type = 7),
  treatment = draw_binary(0.5, N = N),
  proposition_vote = draw_binary(prob = ideology + 1.2 * treatment, link = "probit")
)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(voters[sample.int(nrow(voters), 5, replace = FALSE), c(2, 3, 4, 6, 7, 8, 9)], row.names = FALSE)

## ------------------------------------------------------------------------
library(fabricatr)

panel <- fabricate(
  countries = add_level(N = 150, country_fe = runif(N, 1, 10)),
  years = add_level(N = 25, year_shock = runif(N, 1, 10), nest = FALSE),
  observations = cross_levels(
    by = join(countries, years),
    outcome_it = country_fe + year_shock + rnorm(N, 0, 2)
  )
)


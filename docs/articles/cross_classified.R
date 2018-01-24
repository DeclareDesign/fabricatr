## ---- echo=FALSE---------------------------------------------------------
options(digits=2)
set.seed(19861108)
library(fabricatr)

## ----eval=TRUE, echo=FALSE, fig.width=5, fig.height=3, message=FALSE, error=FALSE----
invisible(library(diagram))
names <- c("Country", "Year", "Observation")
relationship_matrix <- matrix(c(
  0, 0, "",
  0, 0, "",
  0, 0, 0
), ncol = 3, byrow = TRUE)

par(mar = c(0, 0, 0, 0))
plotmat(
  relationship_matrix,
  pos = c(2, 1),
  curve = 0,
  name = names,
  box.type = "square",
  lwd = 1,
  box.lwd = 2,
  cex.txt = 0.8,
  box.size = 0.15,
  box.prop = 0.5
)

## ----eval=FALSE, echo=TRUE, results="hide"-------------------------------
#  panels <- fabricate(
#    countries = add_level(N = 150, country_fe = runif(N, 1, 10)),
#    years = add_level(N = 25, year_shock = runif(N, 1, 10), nest = FALSE),
#    ...
#  )

## ----eval=FALSE, echo=TRUE, results="hide"-------------------------------
#  example_data <- fabricate(
#    list(data_frame_1, data_frame_2),
#    ...
#  )

## ----eval=FALSE, echo=TRUE, results="hide"-------------------------------
#  panels <- fabricate(
#    countries = add_level(N = 150, country_fe = runif(N, 1, 10)),
#    years = add_level(N = 25, year_shock = runif(N, 1, 10), nest = FALSE),
#    obs = cross_levels(
#      by = join(countries, years),
#      new_variable = country_fe + year_shock + rnorm(N, 0, 2)
#    )
#  )

## ----eval=TRUE, echo=FALSE, fig.width=5, fig.height=3, message=FALSE, error=FALSE----
invisible(library(diagram))
names <- c("PrimarySchool", "SecondarySchool", "Student")
relationship_matrix <- matrix(c(
  0, 0, "",
  0, 0, "",
  0, 0, 0
), ncol = 3, byrow = TRUE)

par(mar = c(0, 0, 0, 0))
plotmat(
  relationship_matrix,
  pos = c(2, 1),
  curve = 0,
  name = names,
  box.type = "square",
  lwd = 1,
  box.lwd = 2,
  cex.txt = 0.8,
  box.size = 0.15,
  box.prop = 0.5
)

## ----echo=TRUE, results="hide"-------------------------------------------
schools_data <- fabricate(
  primary_schools = add_level(N = 20, ps_quality = runif(N, 1, 10)),
  secondary_schools = add_level(N = 15, ss_quality = runif(N, 1, 10), nest = FALSE),
  students = link_levels(N = 1500, by = join(primary_schools, secondary_schools))
)

## ----echo=TRUE, results="hide"-------------------------------------------
schools_data <- fabricate(
  primary_schools = add_level(N = 20, ps_quality = runif(N, 1, 10)),
  secondary_schools = add_level(N = 15, ss_quality = runif(N, 1, 10), nest = FALSE),
  students = link_levels(
    N = 1500, by = join(primary_schools, secondary_schools),
    SAT_score = 800 + 13 * ps_quality + 26 * ss_quality +
      rnorm(N, 0, 50)
  )
)

## ----echo=TRUE, results="hide"-------------------------------------------
lm(SAT_score ~ ps_quality + ss_quality, data = schools_data)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(summary(lm(SAT_score ~ ps_quality + ss_quality, data = schools_data))$coefficients)

## ----echo=TRUE, results="hide"-------------------------------------------
corr_data <- fabricate(
  primary_schools = add_level(N = 20, ps_quality = runif(N, 1, 10)),
  secondary_schools = add_level(N = 15, ss_quality = runif(N, 1, 10), nest = FALSE),
  students = link_levels(
    N = 1500, by = join(ps_quality, ss_quality, rho = 0.5),
    SAT_score = 800 + 13 * ps_quality + 26 * ss_quality +
      rnorm(N, 0, 50)
  )
)

## ----echo=TRUE, results="hide"-------------------------------------------
cor(corr_data$ps_quality, corr_data$ss_quality)

## ----echo=TRUE, results="hide"-------------------------------------------
three_data <- fabricate(
  primary_schools = add_level(N = 20, ps_quality = runif(N, 1, 10)),
  secondary_schools = add_level(N = 15, ss_quality = runif(N, 1, 10), nest = FALSE),
  colleges = add_level(N = 50, c_quality = runif(N, 1, 10), nest = FALSE),
  students = link_levels(
    N = 1500,
    by = join(
      ps_quality, ss_quality, c_quality,
      rho = 0.2
    ),
    earning_potential = 20000 + (2000 * ps_quality) +
      (6000 * ss_quality) + (10000 * c_quality) +
      rnorm(N, 0, 5000)
  )
)

## ----echo=TRUE, results="hide"-------------------------------------------
sigma <- matrix(
  c(
    1, 0.4, 0.2,
    0.4, 1, 0.8,
    0.2, 0.8, 1
  ),
  ncol = 3, nrow = 3
)

adv_data <- fabricate(
  primary_schools = add_level(N = 20, ps_quality = runif(N, 1, 10)),
  secondary_schools = add_level(N = 15, ss_quality = runif(N, 1, 10), nest = FALSE),
  colleges = add_level(N = 50, c_quality = runif(N, 1, 10), nest = FALSE),
  students = link_levels(
    N = 1500,
    by = join(
      ps_quality, ss_quality, c_quality,
      sigma = sigma
    ),
    earning_potential = 20000 + (2000 * ps_quality) +
      (6000 * ss_quality) + (10000 * c_quality) +
      rnorm(N, 0, 5000)
  )
)


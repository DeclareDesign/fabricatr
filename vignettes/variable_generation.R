## ---- echo=FALSE---------------------------------------------------------
options(digits=2)
set.seed(19861108)
library(fabricatr)

## ------------------------------------------------------------------------
draw_binary_ex <- fabricate(
  N = 3, p = c(0, .5, 1),
  binary_1 = draw_binary(prob = p),
  binary_2 = draw_binary(N = 3, prob = 0.5)
)

## ------------------------------------------------------------------------
binomial_ex <- fabricate(
  N = 3,
  freethrows = draw_binomial(N = N, prob = 0.5, trials = 10)
)

## ------------------------------------------------------------------------
bernoulli_probit <- fabricate(
  N = 3, x = 10 * rnorm(N),
  binary = draw_binary(prob = x, link = "probit")
)

## ------------------------------------------------------------------------
ordered_example <- fabricate(
  N = 3,
  x = 5 * rnorm(N),
  ordered = draw_ordered(x, breaks = c(-Inf, -1, 1, Inf))
)

## ------------------------------------------------------------------------
ordered_probit_example <- fabricate(
  N = 3,
  x = 5 * rnorm(N),
  ordered = draw_ordered(
    x, breaks = c(-Inf, -1, 1, Inf),
    link = "probit"
  )
)

## ------------------------------------------------------------------------
survey_data <- fabricate(
  N = 100,
  Q1 = draw_likert(x = rnorm(N)),
  Q2 = draw_likert(x = rnorm(N)),
  Q3 = draw_likert(x = rnorm(N))
)

## ----echo=TRUE, results="hide"-------------------------------------------
survey_data <- fabricate(
  N = 100,
  Q1 = draw_likert(x = rnorm(N), type = 7),
  Q2 = draw_likert(x = rnorm(N), type = 5),
  Q3 = draw_likert(x = rnorm(N), type = 4),
  Q4 = draw_likert(x = rnorm(N), breaks = c(-Inf, -0.8, 0, 1, 2, Inf))
)

table(survey_data$Q2)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(as.matrix(t(table(survey_data$Q2))))

## ------------------------------------------------------------------------
count_outcome_example = fabricate(N = 3, 
                                  x = c(0, 5, 100), 
                                  count = draw_count(mean = x))

## ------------------------------------------------------------------------
categorical_example <- fabricate(
  N = 6,
  p1 = runif(N, 0, 1),
  p2 = runif(N, 0, 1),
  p3 = runif(N, 0, 1),
  cat = draw_categorical(N = N, prob = cbind(p1, p2, p3))
)

## ------------------------------------------------------------------------
warn_draw_cat_example <- fabricate(
  N = 6,
  cat = draw_categorical(N = N, prob = c(0.2, 0.4, 0.4))
)

## ----echo=FALSE----------------------------------------------------------
set.seed(456)

## ----echo=TRUE, results="hide"-------------------------------------------
# 100 individual population, 20 each in each of 5 clusters
clusters = rep(1:5, 20)

# Individuals have a 20% chance of smoking, but clusters are highly correlated
# in their tendency to smoke
smoker = draw_binary_icc(prob = 0.2, clusters = clusters, ICC = 0.5)

# Observe distribution of smokers and non-smokers
table(smoker)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(as.matrix(t(table(smoker))))

## ----echo=TRUE, results="hide"-------------------------------------------
table(clusters, smoker)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(table(clusters, smoker))

## ----echo=FALSE----------------------------------------------------------
set.seed(19861108)

## ----echo=TRUE, results="hide"-------------------------------------------
# 100 students, 10 each in 10 clusters
clusters <- rep(1:5, 20)

numeric_grade <- draw_normal_icc(mean = 80, clusters = clusters, ICC = 0.5, sd = 15)

letter_grade <- draw_ordered(
  x = numeric_grade,
  breaks = c(-Inf, 60, 70, 80, 90, Inf),
  break_labels = c("F", "D", "C", "B", "A")
)

mean(numeric_grade)

## ----echo=TRUE, results="hide"-------------------------------------------
table(letter_grade, clusters)

## ----echo=FALSE----------------------------------------------------------
knitr::kable(table(clusters, letter_grade))


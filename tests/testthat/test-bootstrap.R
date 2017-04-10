library(fabricatr)
library(magrittr)

two_levels <- fabricate_data(regions = level(N = 5, gdp = rnorm(N)),
               cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

head(two_levels)

##debugonce(bootstrap_data)
q <- bootstrap_data(two_levels, c(5000, 250), ID_labels = c("regions_ID", "cities_ID"))

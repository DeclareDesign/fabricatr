library(fabricatr)

two_levels <- fabricate_data(regions = level(N = 5, gdp = rnorm(N)),
               cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

head(two_levels)

##debugonce(bootstrap_data)
q <- resample_data(two_levels, c(100, 10), ID_labels = c("regions_ID", "cities_ID"))

q <- resample_data(two_levels, 5)

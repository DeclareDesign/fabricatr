context("Fabricate")

test_that("Fabricate",{

df <- fabricate_data_single_level(N = 2, Y = 10)
df_2 <- fabricate_data_single_level(data = df, Y2 = Y + 1,ID_label = "BOB")

level(ID_label = "test", N = 3, Y = 10)
level(ID_label = "test", N = 4, Y = 10, data = df_2)

# we expect an error here, but need a better explaination (no unnamed arguments)
# fabricate_data_single_level(N = 10, rnorm(N), Y2 = rnorm(N))

level(ID_label = "bob", N = 10, Y1 = rnorm(N), Y2 = rnorm(N))

# can it be called?  YES WE CAN
wrapitup <- function(N){
  level(ID_label = "bob", N = N, Y1 = rnorm(N), Y2 = rnorm(N))
}

wrapitup(12)

#debugonce(fabricate_data)
fabricate_data(N = 100)

fabricate_data(N = 2, Y = 10, ID_label = "test")
fabricate_data(N = 2, Y = 10)

fabricate_data(N = 2, Y1 = rnorm(N), Y2 = rnorm(N))

fabricate_data(Y1 = rnorm(N), Y2 = rnorm(N), data = data.frame(existing_data = rnorm(5)))


fabricate_data(level(N = 5, gdp = rnorm(N), ID_label = "regions"),
               level(N = sample(1:5), subways = rnorm(N, mean = gdp), ID_label = "cities"))

fabricate_data(regions = level(N = 5, gdp = rnorm(N)),
               cities = level(N = sample(1:5), subways = rnorm(N, mean = gdp)))

fabricate_data(regions = level(N = 5),
               cities = level(N = sample(1:5), subways = rnorm(N, mean = 5)))

region_data <- data.frame(capital = c(1, 0, 0, 0, 0))
fabricate_data(regions = level(N = 5, gdp = runif(5)),
               cities = level(N = sample(1:5), subways = rnorm(N, mean = 5)))
})

context("hierarchical")

library(dplyr)
library(magrittr)

test_that("hierarchical data is created correctly when you have a vector variable that is of length N per level",{

  hierarchy <- fabricate_data(
    regions = level(N = 3, gdp = rnorm(N)),
    districts = level(N = 2,
                      var1 = c("recent", "ancient"),
                      var2 = ifelse(var1 == "recent", gdp, 5)),
    cities = level(N = 2, subways = rnorm(N, mean = gdp)))


  expect_equal(hierarchy %>% distinct(regions, var2) %>%
                 group_by(regions) %>% distinct(var2) %>% tally %$% n,
               rep(2, 3))

  expect_equal(hierarchy$var2[hierarchy$var1 == "ancient"], rep(5, 6))

})

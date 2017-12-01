
<!-- README.md is generated from README.Rmd. Please edit that file -->
fabricatr: Imagine your data before you collect it
==================================================

[![Travis-CI Build Status](https://travis-ci.org/DeclareDesign/fabricatr.svg?branch=master)](https://travis-ci.org/DeclareDesign/fabricatr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/DeclareDesign/fabricatr?branch=master&svg=true)](https://ci.appveyor.com/project/DeclareDesign/fabricatr) [![Coverage Status](https://coveralls.io/repos/github/DeclareDesign/fabricatr/badge.svg?branch=master)](https://coveralls.io/github/DeclareDesign/fabricatr?branch=master)

Making decisions about research design and analysis strategies is often difficult before data is collected, because it is hard to imagine the exact form data will take. Instead, researchers typically modify analysis strategies to fit the data. **fabricatr** helps researchers imagine what data will look like before they collect it. Researchers can evaluate alternative analysis strategies, find the best one given how the data will look, and precommit before looking at the realized data.

### Installing fabricatr

To install the latest development release of **fabricatr**, please ensure that you are running version 3.3 or later of R and run the following code:

``` r
install.packages("fabricatr", dependencies = TRUE,
                 repos = c("http://R.declaredesign.org", "https://cloud.r-project.org"))
```

### Getting started

Once the package is installed, it is easy to generate new data, or modify your own. The below example simulates the United States House of Representatives, where 435 members belong to two parties, and both parties and representatives have characteristics modeled in the data:

``` r
library(fabricatr)

house_candidates = fabricate(
  parties = add_level(
    N = 2, 
    party_ideology = c(0.5, -0.5), 
    in_power = c(1, 0), 
    party_incumbents = c(241, 194)),
  representatives = add_level(
    N = party_incumbents, 
    member_ideology = rnorm(N, party_ideology),
    terms_served = draw_discrete(N = N, x = 3, type = "count"),
    female = draw_discrete(N = N, x = 0.2, type = "bernoulli"))
  )
head(house_candidates)
```

| parties |  party\_ideology|  in\_power|  party\_incumbents| representatives |  member\_ideology|  terms\_served|  female|
|:--------|----------------:|----------:|------------------:|:----------------|-----------------:|--------------:|-------:|
| 1       |              0.5|          1|                241| 001             |              1.26|              2|       0|
| 1       |              0.5|          1|                241| 002             |              0.59|              2|       0|
| 1       |              0.5|          1|                241| 003             |              0.05|              2|       1|
| 1       |              0.5|          1|                241| 004             |              0.02|              3|       0|
| 1       |              0.5|          1|                241| 005             |              1.54|              6|       0|
| 1       |              0.5|          1|                241| 006             |             -0.50|              4|       0|

For more information, use the command `?fabricate` in R to explore our documentation or read our [online tutorial](http://fabricatr.declaredesign.org/articles/getting_started.html).

This project is generously supported by a grant from the [Laura and John Arnold Foundation](http://www.arnoldfoundation.org) and seed funding from [EGAP](http://egap.org).

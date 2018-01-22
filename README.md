
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

Once you have installed **fabricatr**, you can easily import your own data or generate new data. **fabricatr** is designed to help you solve two key problems:

1.  Generating variables that look like the real thing, including Likert survey responses, treatment status, demographic variables, and variables correlated by group.
2.  Generating data that are structured like the real thing, including multi-level ("nested") data or cross-classified data.

**fabricatr** is easy to learn and easy to read. Consider this example which generates data modeling the United States House of Representatives:

``` r
library(fabricatr)

house_members = fabricate(
  party_id = add_level(
    N = 2, 
    party_names = c("Republican", "Democrat"),
    party_ideology = c(0.5, -0.5), 
    in_power = c(1, 0), 
    party_incumbents = c(241, 194)),
  rep_id = add_level(
    N = party_incumbents, 
    member_ideology = rnorm(N, party_ideology, sd=0.5),
    terms_served = draw_count(N = N, mean = 4),
    female = draw_binary(N = N, prob = 0.198))
  )
```

|     | party\_names |  party\_ideology|  in\_power|  member\_ideology|  terms\_served|  female|
|-----|:-------------|----------------:|----------:|-----------------:|--------------:|-------:|
| 339 | Democrat     |             -0.5|          0|              0.11|              3|       0|
| 217 | Republican   |              0.5|          1|             -0.37|              1|       0|
| 233 | Republican   |              0.5|          1|              0.71|              2|       1|
| 263 | Democrat     |             -0.5|          0|             -1.05|              3|       0|
| 140 | Republican   |              0.5|          1|              0.24|              4|       0|

### Next Steps

For more information, read our [online tutorial](http://fabricatr.declaredesign.org/articles/getting_started.html) to get started with **fabricatr**. This tutorial will give you a brief overview of **fabricatr**'s main functions and direct you towards your next steps. You can also read our documentation inside R using the command `?fabricate` as your entry point.

This project is generously supported by a grant from the [Laura and John Arnold Foundation](http://www.arnoldfoundation.org) and seed funding from [EGAP](http://egap.org).

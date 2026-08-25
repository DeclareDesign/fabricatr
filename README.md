# fabricatr

fabricatr makes the data you do not have yet: you describe the variables and how they relate, and `fabricate()` builds a data frame that looks like what you expect to collect, so a research design can be simulated and diagnosed before there is anything to analyze. It handles hierarchical and cross-classified data, potential outcomes, and draws with a target intra-cluster correlation.

```r
library(fabricatr)
fabricate(
  villages = add_level(N = 5, wealth = rnorm(N)),
  citizens = nest_level(N = 4, income = wealth + rnorm(N))
)
```

## Installation

fabricatr 2.0 is not yet on CRAN. Until it is, install it from the `rewrite` branch:

```r
# install.packages("remotes")
remotes::install_github("DeclareDesign/fabricatr@rewrite", build_vignettes = TRUE)
```

The branch installs under the released name, so it replaces the CRAN fabricatr in your library. To keep 1.0.2 for comparison, install it into a separate library and load it with `lib.loc`.

## What changed in 2.0

2.0 is a rewrite of the package on dplyr, tibble, purrr, and rlang, with the same exported functions as 1.0.2. A script written for 1.x runs, and each old spelling warns once with the call to write instead. `vignette("fabricatr2.0")` has the full account; the short list:

* `add_level(nest = FALSE)` becomes `declare_level()`.
* `by = join_using(A, B)` becomes `.by = c("A", "B")` in `cross_levels()` and `link_levels()`, and `rho` is an ordinary argument of `link_levels()`.
* `modify_level(by =)` becomes `modify_level(.by =)`, and `modify_level()` named after a level works at that level while keeping the levels nested inside it.
* `break_labels` and `category_labels` become `labels`.
* `N` is supplied by name: `fabricate(N = 100, ...)`, never `fabricate(100, ...)`.
* `total_sd` in `draw_normal_icc()` is a parameter of the draw, not a rescaling, so the same call gives different numbers.
* The correlated draw in `link_levels()` takes one code path on every machine, with or without `mvnfast`.
* Levels are two to four times faster to build, which is where design diagnosis spends its time.

## Where to read next

`vignette("getting-started")` is a first tour for someone new to the package. The book at <https://book.declaredesign.org> puts fabricated data to work in complete research designs.

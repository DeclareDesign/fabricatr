# fabricatr 2.0.0

fabricatr 2.0.0 is a rewrite of the package on dplyr, tibble, purrr, and rlang, with the same exported functions as 1.0.2. The vignette `vignette("fabricatr2.0")` walks through every difference; the list below is the summary.

## Breaking changes

* `N` is supplied by name. `fabricate(100, Y = rnorm(N))` was read as `N = 100` in 1.x and is now an error whose message says to write `fabricate(N = 100, Y = rnorm(N))`.

* Every expression passed to `fabricate()` or to a level needs a name. An unnamed one, `fabricate(N = 5, rnorm(N))`, is an error naming its position; 1.x failed on it with an internal indexing error.

* `link_levels()` is `link_levels(N, .by, ..., rho = 0, sigma = NULL)`. `rho` and `sigma` sit behind the dots, so they must be named and a column called `r` or `s` is no longer partially matched into them. In 1.x they were arguments of `join_using()`.

* `draw_normal_icc()` and `draw_binary_icc()` take `clusters` first: `draw_normal_icc(clusters, ICC, mean, sd, sd_between, total_sd, N)` and `draw_binary_icc(clusters, prob, ICC, N)`. Positional calls written for 1.x's order (`mean, N, clusters, ...` and `prob, N, clusters, ICC`) need names.

* `draw_ordered()` requires `breaks`; 1.x defaulted it to `c(-1, 0, 1)`.

* `draw_categorical()` drops `latent` and `link`, which 1.x accepted but rejected for any link other than identity.

* `total_sd` in `draw_normal_icc()` is a parameter of the draw rather than a rescaling of the finished vector, so the realised `sd()` varies from draw to draw as any sample statistic does. The same call gives different numbers than 1.x (fabricatr#133).

* `link_levels()` with `rho` or `sigma` takes one draw path, `chol()` on the correlation matrix, on every machine. 1.x used `mvnfast::rmvn()` when that package was installed, so on such a machine the same seed gives different numbers than 1.x.

* `modify_level()` named after a level above the current one, `regions = modify_level(z = rnorm(N))` after a `cities` level, evaluates once per region as 1.x does, and then keeps the cities. 1.x returned the regions frame alone, dropping the nested rows.

## Deprecations

Each of these is accepted and warns once per call site with the call to write instead.

* `add_level(..., nest = FALSE)` is `declare_level(...)`, and `nest = TRUE` is `add_level(...)`.

* `cross_levels(by = join_using(A, B))` and `link_levels(by = join_using(A, B))` are `.by = c("A", "B")`. A `rho` inside `join_using()` is carried through to `link_levels(rho = )`.

* `modify_level(..., by = "g")` is `modify_level(..., .by = "g")`.

* `draw_ordered(break_labels = )` and `draw_categorical(category_labels = )` are `labels = `.

* `join_using()` and `recycle()` are kept so that 1.x code runs; neither is needed in new code.

## New

* `declare_level()` builds an independent level for `cross_levels()` and `link_levels()`, replacing `add_level(nest = FALSE)`.

* `n()` inside any declaration is the number of rows the level is building, the same count `N` names, as a function call that cannot be confused with a parameter of the design.

* Level calls and ordinary columns can be mixed in one `fabricate()` call. 1.x required all of the arguments to be level calls or none of them.

* `modify_level(.by = )` groups the rows in hand by a column before evaluating, on flat data as well as on levels.

* `draw_ordered()`, `draw_likert()`, and `draw_categorical()` take `labels`. `draw_ordered()` and `draw_likert()` return an ordered factor when it is given, and `draw_categorical()` an unordered one, as in 1.x.

* `draw_likert()` accepts a manual `breaks` vector.

* `draw_normal_icc()` accepts any two of `ICC`, `sd`, `sd_between`, and `total_sd`, and the full `ICC` range including 0 and 1 (fabricatr#149).

* `correlate()` works with base R random number generators such as `rnorm()`, not only the `draw_*` family.

* `link_levels()` validates `sigma`: wrong dimensions, entries outside `[-1, 1]`, asymmetry, and a matrix that is not positive semi-definite are errors rather than a silently uncorrelated draw.

## Fixes against 1.0.2

* A matrix-valued column is split into `X.1`, `X.2`, ... as it is stored, so a later expression can read `X.1` (fabricatr#188).

* `draw_binary_icc()` pairs each cluster with its own probability when `prob` has one value per unit, and errors when a cluster's probabilities disagree (fabricatr#189).

* `fabricate(N = 2.5)` and `N = 0` are errors, as in 1.0.2, rather than a silently truncated row count.

* `fabricate(data = df, N = 10)` is an error: the data fix the number of rows.

* `draw_quantile()` returns an unordered factor, as 1.x does, so a model formula picks treatment contrasts rather than polynomial ones.

* `resample_data(unique_labels = TRUE)` builds labels matching 1.x column for column.

* ID columns are zero-padded character strings at every level, padded to the number of units at that level, as in 1.x.

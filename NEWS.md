# fabricatr 2.0.0

fabricatr 2.0.0 is a rewrite of the package on dplyr, tibble, purrr, and rlang, with the same exported functions as 1.0.2. The vignette `vignette("fabricatr2.0")` walks through every difference; the list below is the summary.

## Breaking changes

* `fabricate()` returns a tibble where 1.0.2 returned a plain data frame. The columns, their order, their types, and their values are unchanged, and a tibble is a data frame, so anything that accepts one still works. Two things differ for a caller: `identical()` against a plain data frame is now `FALSE`, and `df[, "Y"]` returns a one-column tibble rather than dropping to a vector. `df$Y` and `df[["Y"]]` are unchanged.

* `N` is supplied by name. `fabricate(100, Y = rnorm(N))` was read as `N = 100` in 1.x and is now an error whose message says to write `fabricate(N = 100, Y = rnorm(N))`.

* Every expression passed to `fabricate()` or to a level needs a name. An unnamed one, `fabricate(N = 5, rnorm(N))`, is an error naming its position; 1.x failed on it with an internal indexing error.

* `link_levels()` is `link_levels(N, .by, ..., rho = 0, sigma = NULL)`. `rho` and `sigma` sit behind the dots, so they must be named and a column called `r` or `s` is no longer partially matched into them. In 1.x they were arguments of `join_using()`.

* `draw_normal_icc()` and `draw_binary_icc()` take `clusters` first: `draw_normal_icc(clusters, ICC, mean, sd, sd_between, total_sd, N)` and `draw_binary_icc(clusters, prob, ICC, N)`. Positional calls written for 1.x's order (`mean, N, clusters, ...` and `prob, N, clusters, ICC`) need names.

* `draw_ordered()` requires `breaks`; 1.x defaulted it to `c(-1, 0, 1)`.

* `draw_likert(breaks = )` reads interior cut-points, as `draw_ordered(breaks = )` does, so `draw_likert(x, breaks = c(-1, 0, 1))` gives four categories where 1.0.2 gave two. 1.0.2 handed `breaks` to `cut()`, which reads the vector as the full set of bin boundaries and sends every value outside it to `NA`: on 200 draws from `rnorm()` the same call returned two categories and 53 `NA`. `draw_likert()` is a convenience wrapper around `draw_ordered()`, and in 1.0.2 the two read the same argument name in two different ways, since `draw_ordered(x, breaks = c(-1, 0, 1))` gives four categories in 1.0.2 and in 2.0 alike. To ask for bin boundaries, say so: `draw_ordered(x, breaks = c(-1, 0, 1), strict = TRUE)` returns 1.0.2's two categories and its 53 `NA`.

* `draw_likert(min = , max = , bins = )` puts values outside `[min, max]` in the outermost bins rather than returning `NA` for them. `min` and `max` declare the range the latent variable is cut on, not a filter, and 1.0.2's `NA` was missing data in a Likert item that nothing reported: `draw_likert(x, min = -3, max = 3, bins = 5)` on 200 draws from `rnorm()` returned one. A value exactly equal to `min` was `NA` too, because `cut()` leaves its lowest boundary open. The same recipe recovers 1.0.2 exactly: `draw_ordered(x, breaks = seq(min, max, length.out = bins + 1), strict = TRUE)`.

* `draw_likert()` returns integer codes where 1.0.2 returned doubles, which it got by passing `cut()`'s factor through `as.numeric()`. The codes are the same numbers, so only `identical()` and `typeof()` see the difference. `draw_ordered()` returned integer codes in 1.0.2 and still does.

* `draw_categorical()` drops `latent` and `link`, which 1.x accepted but rejected for any link other than identity.

* `total_sd` in `draw_normal_icc()` is a parameter of the draw rather than a rescaling of the finished vector, so the realised `sd()` varies from draw to draw as any sample statistic does. The same call gives different numbers than 1.x (fabricatr#133).

* `modify_level()` named after a level above the current one, `regions = modify_level(z = rnorm(N))` after a `cities` level, evaluates once per region as 1.x does, and then keeps the cities. 1.x returned the regions frame alone, dropping the nested rows.

* `reveal_outcomes()` keeps the type of the potential outcomes it reads. A factor reveals as a factor with its levels and their order intact, and a `Date` as a `Date`. 1.x selected the revealed value by matrix-indexing, which rendered both as character: an outcome declared `lo < mid < hi` came back as character, so re-factoring it ordered the levels alphabetically as `hi < lo < mid` and a model fitted on it used the wrong baseline in silence. Where the conditions do not agree on one set of factor levels there are none to keep, so the result is character and a warning says so. This is a deliberate break with 1.0.2.

## Reproducing an old seed

fabricatr does not promise that a given seed produces the same fabricated data in 2.0 as it did in 1.x, and two things here land at a different point in the random number stream. The distribution each one draws from is unchanged; only the position moves. This is deliberate: fabricatr invents data to stand in for data you do not have, so which particular draw you got is not a finding. Contrast randomizr, where the draw assigns real units to real treatments and is therefore part of the result, and whose stream does not move across versions. What 2.0 does promise is that it gives the same numbers as itself on every machine, which 1.x could not.

If a saved analysis draws its data through `fabricate()` under a stored seed, expect its numbers to move; re-run it and store the new ones.

* `link_levels()` with `rho` or `sigma` takes one draw path, `chol()` on the correlation matrix, on every machine. 1.x used `mvnfast::rmvn()` when that package was installed, and passed it a core count, so its numbers depended on both. Without `mvnfast` installed, 1.x and 2.0 agree exactly.

* `fabricate(data = df, ...)` no longer advances the stream before evaluating your first column. 1.x named the workspace slot it put `df` into with `sample.int(.Machine$integer.max, 1)`, which costs exactly two uniforms, so every 1.x call starting from data began two draws in.

## Deprecations

Each of these is accepted and warns once per call site with the call to write instead.

* `add_level(..., nest = FALSE)` is `declare_level(...)`, and `nest = TRUE` is `add_level(...)`.

* `cross_levels(by = join_using(A, B))` and `link_levels(by = join_using(A, B))` are `.by = c("A", "B")`. A `rho` inside `join_using()` is carried through to `link_levels(rho = )`. R will not partial-match a supplied `by =` to a formal named `.by`, so `.by` is left unfilled and R binds the first *unnamed* argument to it positionally: `cross_levels(by = join_using(A, B), potential_outcomes(Y ~ Z))` put the potential outcomes in `.by`, where the shim discarded them without a warning and the design ran on without its potential outcomes. The displaced argument is now put back where the author wrote it, so the two spellings of such a call return the same data.

* `modify_level(..., by = "g")` is `modify_level(..., .by = "g")`.

* `draw_ordered(break_labels = )` and `draw_categorical(category_labels = )` are `labels = `.

* `join_using()` and `recycle()` are kept so that 1.x code runs; neither is needed in new code.

## New

* `declare_level()` builds an independent level for `cross_levels()` and `link_levels()`, replacing `add_level(nest = FALSE)`.

* `import_level()` brings an existing data frame into a `fabricate()` call as a level of its own, so two real data sets can be crossed or linked (fabricatr#165). The level's ID is the frame's own key: a column named after the level is used as it stands, the way a join matches on a shared name, `.id` names one that is called something else, and a frame with no key of its own gets numbered rows. An imported key keeps its own type and values, so it still matches the data it came from, and a key with a repeated or missing value is refused. `add_level(data = df)`, the spelling the issue was filed on and one that has never imported anything in either version, now fails with a message naming `import_level()`. A column called `N` is refused, because `N` is the row count in every expression a level evaluates and the column would leave the frame in silence; `fabricate(data = )` evaluates nothing at a level and still keeps it.

* `fabricate_with_dots()` is exported. It takes an already-captured list of quosures and builds what `fabricate()` would have built from the same expressions, which is what a package whose own verbs capture their arguments needs: splicing captures back through `!!!` renders them as formulas and `fabricate()` captures those again, one level too deep. It existed as an internal in 2.0's development and DeclareDesign reached it with `:::`, which made the boundary between the two packages depend on nothing but the two of them sharing a maintainer, since `R CMD check` suppresses the unexported-object NOTE in that case alone.

* `n()` inside any declaration is the number of rows the level is building, the same count `N` names, as a function call that cannot be confused with a parameter of the design.

* Level calls and ordinary columns can be mixed in one `fabricate()` call. 1.x required all of the arguments to be level calls or none of them.

* `modify_level(.by = )` groups the rows in hand by a column before evaluating, on flat data as well as on levels.

* `ID_label = NA` suppresses the unit ID column, leaving the rows it would have numbered. 1.x accepted the argument and then failed with an indexing error, so there was no way to ask for a frame without one.

* `draw_ordered()`, `draw_likert()`, and `draw_categorical()` take `labels`. `draw_ordered()` and `draw_likert()` return an ordered factor when it is given, and `draw_categorical()` an unordered one, as in 1.x.

* `draw_likert()` reports the number of categories a manual `breaks` vector produces when `labels` does not match it, rather than failing inside `cut()`.

* `draw_normal_icc()` accepts any two of `ICC`, `sd`, `sd_between`, and `total_sd`, and the full `ICC` range including 0 and 1 (fabricatr#149).

* `draw_count()` takes `dispersion` for overdispersed counts. The default 0 is the Poisson draw it has always made, on the same random number stream. A positive value draws from the negative binomial with the same mean and variance `mean + dispersion * mean^2`, the parameterization of Stata's `nbreg` and the reciprocal of `MASS::glm.nb()`'s `theta`. The link, the latent scale, and `quantile_y` work as before, so an overdispersed count can be correlated with another variable through `correlate()`.

* `draw_normal_ar()` draws an AR(1) process inside each unit of a panel, the serially correlated errors that make unclustered standard errors wrong in difference-in-differences. Each unit's series starts at its stationary distribution, so every period has the same variance, and a gap in `time` decays the correlation as `rho` raised to the gap, so an unbalanced panel needs no special handling. `time` may be the character ID that `cross_levels()` gives a level, or a `Date`, read in days. `sd` is the standard deviation of the process in each period, not of each period's new shock as in `stats::arima.sim()`, so changing `rho` leaves the variance where it was. Binary and count outcomes with the same persistence come from passing the draw on as `latent` or `quantile_y`.

* `correlate()` works with base R random number generators such as `rnorm()`, not only the `draw_*` family.

* `link_levels()` validates `sigma`: wrong dimensions, entries outside `[-1, 1]`, asymmetry, and a matrix that is not positive semi-definite are errors rather than a silently uncorrelated draw.

## Fixes against 1.0.2

* A matrix-valued column is split into `X.1`, `X.2`, ... as it is stored, so a later expression can read `X.1` (fabricatr#188).

* `draw_binary_icc()` pairs each cluster with its own probability when `prob` has one value per unit, and errors when a cluster's probabilities disagree (fabricatr#189).

* `fabricate(N = 2.5)` and `N = 0` are errors, as in 1.0.2, rather than a silently truncated row count.

* `fabricate(data = df, N = 10)` is an error: the data fix the number of rows.

* `draw_quantile()` returns an unordered factor, as 1.x does, so a model formula picks treatment contrasts rather than polynomial ones.

* `draw_ordered()` numbers its categories from 1 whatever `breaks` looks like. An infinite endpoint bounds the scale rather than cutting it, so `c(-Inf, 0, Inf)` gives the same two categories as `0`, and `strict = TRUE` drops the open end categories instead of leaving them empty and unreachable. 1.0.2 reads the lower end the same way but applies the same test to a trailing `Inf`, so `c(-1, 0, Inf)` came back 0-based; only the lower end decides, since `findInterval()` returns 0 for a value below the first break and nothing else. `labels` now takes one label per category that can occur, which is `length(breaks) + 1` for interior cut-points and one fewer for each infinite endpoint.

* `draw_likert(labels = )` attaches each label to its own category. 1.0.2 built the factor with `levels = unique(x_ret)`, the order the categories happen to appear in the data rather than their sorted order, so each label landed on whichever category turned up first: `draw_likert(c(2.5, 1.5, 0, -1.5, -2.5), min = -3, max = 3, bins = 5, labels = c("SD", "D", "N", "A", "SA"))` labelled the highest value `SD` and the lowest `SA`, inverting the scale in silence. When fewer than `bins` categories occurred it failed outright, with `invalid 'labels'; length 5 should be 1 or 3` raised from inside `factor()`.

* `resample_data(unique_labels = TRUE)` builds labels matching 1.x column for column.

* ID columns are zero-padded character strings at every level, padded to the number of units at that level, as in 1.x.

* An `NA` in `given` propagates to the result of `correlate()` rather than being absorbed into it. `rank()` puts an `NA` last by default, so 1.0.2 gave the missing value the highest rank and drew a correspondingly extreme value to match: the returned vector held no `NA` anywhere and the missingness read as a real observation. The observed values are now ranked among themselves and each `NA` comes back as an `NA`.

* Argument validation names the argument and the function rather than failing inside a comparison. An `NA` supplied as `rho` to `correlate()`, as `prob` or `N` to `draw_categorical()`, as `ICC` to `draw_binary_icc()`, or as `min`, `max`, or `bins` to `draw_likert()` reached an `if()` still `NA` and raised R's own `missing value where TRUE/FALSE needed`, which names neither. A fractional `type` in `split_quantile()` and `draw_quantile()` is an error rather than `number of intervals and length of 'labels' differ` raised from inside `cut()`.

* `modify_level(.by = )` names the argument when it is not a column name. `.by` is an ordinary argument, so a bare column name evaluates to the column and arrives as a vector, while the 1.x spelling `by = ` is captured unevaluated and does take a bare name: the two differ, and `modify_level(m = mean(Y), .by = g)` reached the subscript and failed there with `no such index at level 1`. A column name that is not in view gave `argument 1 is not a vector`, two names gave `subscript out of bounds`, and a number silently grouped by whichever column came first. `cross_levels()` and `link_levels()` already named their function and their `.by`.

* An unrecognised `link` in `draw_binary()` reports `draw_binary()`. It delegates to `draw_binomial()`, which validated the name under its own label, so the error named a function the caller had not written. 1.0.2 named no function at all and listed a set of valid links that omitted `logistic`.

* Crossing or linking a level with one it is nested inside is refused with a message naming the shared columns. `add_level()` nests, so the second level's entry carries the first's columns, and joining the two duplicated them: the call failed several steps later on a name-uniqueness check that named neither level. 1.0.2 refuses the same call, saying the level name is ambiguous. Two independent levels that happen to define the same column name are refused on the same grounds.

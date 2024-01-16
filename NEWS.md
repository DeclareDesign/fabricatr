# **fabricatr** 1.0.2

- Minor changes to stay current on CRAN.

# **fabricatr** 1.0.0

- `draw_categorical()` now returns a factor when `category_labels` is specified.
- `draw_ordered()` now returns an ordered factor when `break_labels` is specified.
- `draw_ordered()` performance is improved when returning a factor.

# **fabricatr** 0.16.0

- Replacement `draw_likert` function.
- `join` function renamed to `join_using` to avoid confusion with other R functions and to more clearly indicate its purpose.
- Bug fixes.

# **fabricatr** 0.14.0

- Bug fixes

# **fabricatr** 0.12.0

- Added potential_outcomes (creates multiple columns of potential outcomes according to a formula)
- Added reveal_outcomes (reveals observed outcomes on the basis of potential outcomes and a treatment assignment)
- Added draw_multivariate (interface for adding correlated draws to fabricate calls)

# **fabricatr** 0.10.0

- Changes for compatibility with `rlang` 0.4.0
- Minor bug fixes

# **fabricatr** 0.8.0

- Changes to prepare for R 3.6.0

# **fabricatr** 0.6.0

- Added split-modify-combine functionality to `modify_level` via the `by` argument.
- Changed behavior of `draw_ordered` to ensure categories are data-independent and adding a `strict` option that replaces values with `NA` if values are outside all breaks.
- Labeled `correlate` functionality as experimental.
- Many bug fixes.

# **fabricatr** 0.4.0

- `recycle()` helper function for expanding and recycling data
- Added `correlate()` function to allow users to generate arbitrary correlated random variables.
- Added `category_labels` argument to `draw_categorical`
- Added `draw_quantile` and `split_quantile` functions
- Major updates to test harness
- Changed ID label stapling behaviour in `fabricate` calls.
- Allow users to pass `ID_label` in level creation calls to allow back-door unnamed level calls.
- Bugfix for constants in certain `modify_level` contexts.
- Changed specification of latent variables with links; users should now provide `latent` as an argument when using a link.
- New syntax for using `length(ID)` from upper level to define `N` of lower level.
- When passing a vector to `draw_categorical` prob argument, package now sends message instead of warning.
- Major additions to documentation and vignettes, including vignettes for common social sciences variables, time series, and using other creation packages with fabricatr.
- Bugfix in certain cases with single-level variables autocompleting to "data" and not working.
- Added `unique_labels` argument to `resample_data` to allow block-level statistic calculation.

# **fabricatr** 0.2.0 

First CRAN submission for **fabricatr**

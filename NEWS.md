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

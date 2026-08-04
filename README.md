# fabricatr, rewritten

**This branch is not the CRAN package and it is not `main`.** It holds a ground-up rewrite of fabricatr on tidyverse foundations. The released fabricatr is unaffected by anything here.

The package on this branch is still named `fabricatrZero`, so installing it leaves your CRAN fabricatr in place and both can be loaded in the same session.

```r
remotes::install_github("DeclareDesign/fabricatr@rewrite", build_vignettes = TRUE)
vignette("fabricatrZero")
```

The vignette is the document to read first. It covers what does not change, what changes and why, how to port an existing script, and the speed measurements.

## What it is

1,802 lines of R against fabricatr's 3,193, built on dplyr, tibble and purrr rather than the workspace system. The exported API is a superset of fabricatr's: every function fabricatr exports is here, plus `declare_level()`. The fabricatr spellings (`nest = FALSE`, `by =`, `join_using()`, `recycle()`) are accepted and deprecated, each warning printing the call to write instead, built from the caller's own arguments.

## Status

As of 2026-07-30: 212 tests passing, `R CMD check` 0 errors / 0 warnings / 0 notes.

The intent is that this becomes fabricatr 2.0.0, at which point the package is renamed and the vignette becomes a migration guide. That has not happened yet, and nothing on this branch asserts it: the DESCRIPTION still reads `fabricatrZero 0.1.0`.

Sibling branches: `DeclareDesign/estimatr@rewrite` and `DeclareDesign/DeclareDesign@rewrite`.

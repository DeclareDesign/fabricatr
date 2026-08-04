# fabricatr, rewritten

**This branch is not the CRAN package and it is not `main`.** It holds a ground-up rewrite of fabricatr on tidyverse foundations. The released fabricatr is unaffected by anything here.

The package on this branch is still named `fabricatrZero`, so installing it leaves your CRAN fabricatr in place and both can be loaded in the same session.

```r
remotes::install_github("DeclareDesign/fabricatr@rewrite", build_vignettes = TRUE)
vignette("fabricatr2.0")
```

The vignette is the document to read first. It covers what does not change, what changes and why, how to port an existing script, and the speed measurements.

## What it is

1,802 lines of R against fabricatr's 3,193, built on dplyr, tibble and purrr rather than the workspace system. The exported API is a superset of fabricatr's: every function fabricatr exports is here, plus `declare_level()`. The fabricatr spellings (`nest = FALSE`, `by =`, `join_using()`, `recycle()`) are accepted and deprecated, each warning printing the call to write instead, built from the caller's own arguments.

## Status

As of 2026-07-30: 212 tests passing, `R CMD check` 0 errors / 0 warnings / 0 notes.

**This branch is versioned 2.0.0**, against fabricatr 1.0.2 on CRAN. It is still *named* `fabricatrZero`, and that is the deliberate part: the version says where it is going, the name is what keeps it installable alongside the released package while the two are still being compared. The rename is the last step before release, and it is the one that cannot be undone cheaply, because two packages of one name cannot be loaded together.

Sibling branches: `DeclareDesign/estimatr@rewrite` and `DeclareDesign/DeclareDesign@rewrite`.

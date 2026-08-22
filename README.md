# fabricatr, rewritten

**This branch is not the CRAN package and it is not `main`.** It holds a ground-up rewrite of fabricatr on tidyverse foundations. The released fabricatr is unaffected by anything here.

The package on this branch is named `fabricatr`, the same as the released one, so installing it **replaces** your CRAN fabricatr. Two packages of one name cannot be loaded together. To keep the released version available for comparison, install it into a separate library and pass `lib.loc` when you need it.

```r
remotes::install_github("DeclareDesign/fabricatr@rewrite", build_vignettes = TRUE)
vignette("fabricatr2.0")
```

The vignette is the document to read first. It covers what does not change, what changes and why, how to port an existing script, and the speed measurements.

## What it is

1,802 lines of R against fabricatr's 3,193, built on dplyr, tibble and purrr rather than the workspace system. The exported API is a superset of fabricatr's: every function fabricatr exports is here, plus `declare_level()`. The fabricatr spellings (`nest = FALSE`, `by =`, `join_using()`, `recycle()`) are accepted and deprecated, each warning printing the call to write instead, built from the caller's own arguments.

## Status

As of 2026-07-30: 212 tests passing, `R CMD check` 0 errors / 0 warnings / 0 notes.

**This branch is versioned 2.0.0**, against fabricatr 1.0.2 on CRAN. It carries the released package's name, so it is a drop-in replacement rather than something that sits beside it.

Sibling branches: `DeclareDesign/estimatr@rewrite` and `DeclareDesign/DeclareDesign@rewrite`.

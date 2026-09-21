> **HOLD. Do not submit yet.** Two things are outstanding: Graeme Blair has not
> yet written to CRAN confirming the maintainer transfer, and win-builder has
> not been run. Delete this banner once both are done and the
> "Test environments" section names the win-builder reports.

## Submission

fabricatr 2.0.0 is a rewrite of the package on dplyr, tibble, purrr, and rlang. Every function 1.0.2 exported is still exported, under the same name. Five argument spellings change and each old spelling is accepted with a deprecation warning naming the call to write instead, so a script written for 1.x still runs. Two changes are hard: a positional `N`, `fabricate(100, ...)`, is now an error whose message says to write `fabricate(N = 100, ...)`, and every expression passed to `fabricate()` must be named. The full list is in NEWS.md and each item has its own section in `vignette("fabricatr2.0")`.

**This submission changes the maintainer** from Graeme Blair <graeme.blair@gmail.com> to Alexander Coppock <acoppock@gmail.com>. Graeme Blair has written to CRAN separately to confirm the transfer. He remains an author. Nothing else in `Authors@R` changes: all seven people listed by 1.0.2 are listed here with the roles they had.

This version was written by the maintainers working with AI assistance (Claude, from Anthropic), and `vignette("fabricatr2.0")` says so. The evidence for the release: a suite of 331 assertions, a section-by-section comparison against 1.0.2 in that vignette with every example run under both packages and compared row by row, and a benchmark harness in `data-raw/` that re-measures each speed claim the vignette makes and fails if one has gone stale.

## Test environments

* local macOS 26.6 (aarch64, Apple M4), R 4.6.0
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest (release), windows-latest (release). All five green at `FAIL 0 | WARN 0 | SKIP 0 | PASS 331` (run 35552743496).
* win-builder: not yet run

The package has no `skip_on_cran()`, so the suite does not split on `NOT_CRAN`: all five CI platforms and the local check report the same `FAIL 0 | WARN 0 | SKIP 0 | PASS 331`, and the zero skips are read off each job's log rather than inferred from the green badge.

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alexander Coppock <acoppock@gmail.com>'

New maintainer:
  Alexander Coppock <acoppock@gmail.com>
Old maintainer(s):
  Graeme Blair <graeme.blair@gmail.com>
```

The maintainer change is intentional and is covered by Graeme Blair's separate message to CRAN.

## Reverse dependencies

`revdepcheck` was run against the submitted code on 2026-09-20: 5 checked, 3 clean, 2 broken, none failed to check. `CausalQueries`, `osdc`, and `randomizr` are unaffected. The two that break are `DeclareDesign` 1.1.1 and `DesignLibrary` 0.1.10, both maintained by us, and both breaks are the documented 2.0 changes rather than anything unintended. Replacements for both are ready: `DeclareDesign` 2.0.0 and `DesignLibrary` 0.2.0 will be submitted immediately after this release, and we would rather submit them in that order than hold 2.0.0 back, since each of them depends on this package.

`DesignLibrary` 0.1.10 fails five test assertions, all of them one call: `factorial_designer()` and `two_by_two_designer()` build their data with a positional `N`, which 2.0 refuses by design. The refusal is the change NEWS lists first and its message names the call to write instead.

`DeclareDesign` 1.1.1 fails six, and none of the six concerns a value. Five compare a fabricated frame to a plain data frame with `identical()` or `expect_equal()`, and now see a `tbl_df`: the columns, their order, their types, and their values agree, and only the class attribute differs. The sixth asserts the dimension of a diagnosis table built from five simulations, with a grouping variable defined as `ifelse(p.value > 0.1, NA, p.value <= 0.05)`. Whether any of the five simulations lands on the non-`NA` side of that cut decides whether the grouping variable has one level or two, so the expected dimension holds only at the seed the test happened to reach. fabricatr 2.0 draws from the same distributions as 1.0.2 but lands at a different point in the random number stream in two places, both documented under "Reproducing an old seed" in NEWS.md, and that is what moves this test. Run under 2.0 at five seeds, the table comes back with 7 rows at four of them and 14 at the fifth.

An earlier run of this release, on 2026-09-20, broke `DeclareDesign` in two further places with `object 'N' not found`. That one was ours: 1.0.2 reads a leading unnamed data frame as `data`, and the rewrite had been splicing it in as columns instead, so `N` and `n()` were never bound. It is the shape every wrapper produces, since forwarding through `...` cannot name the argument. Fixed, with the contract pinned by test.

# DeclareDesign (1.1.1)

* GitHub: <https://github.com/DeclareDesign/DeclareDesign>
* Email: <mailto:acoppock@gmail.com>
* GitHub mirror: <https://github.com/cran/DeclareDesign>

Run `revdepcheck::revdep_details(, "DeclareDesign")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
       ── Failure ('test-population.R:114:3'): use custom data with declare_model ─────
       Expected `region_data` to be identical to `d2`.
       Differences:
       Attributes: < Component "class": Lengths (1, 3) differ (string compare on first 1) >
       Attributes: < Component "class": 1 string mismatch >
       ── Failure ('test-potential-outcomes.R:118:3'): POs at a higher level ──────────
       Expected `head(my_potential_outcomes_discrete(pop))` to equal `structure(...)`.
       Differences:
       Attributes: < Component "class": Lengths (3, 1) differ (string compare on first 1) >
       Attributes: < Component "class": 1 string mismatch >
       
       [ FAIL 6 | WARN 6 | SKIP 7 | PASS 641 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking whether package ‘DeclareDesign’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: package ‘randomizr’ was built under R version 4.6.1
       Warning: package ‘estimatr’ was built under R version 4.6.1
     See ‘/Users/alexandercoppock/git_projects/fabricatr/revdep/checks.noindex/DeclareDesign/new/DeclareDesign.Rcheck/00install.out’ for details.
     ```

# DesignLibrary (0.1.10)

* GitHub: <https://github.com/DeclareDesign/DesignLibrary>
* Email: <mailto:jjc2247@columbia.edu>
* GitHub mirror: <https://github.com/cran/DesignLibrary>

Run `revdepcheck::revdep_details(, "DesignLibrary")` for more info

## Newly broken

*   checking tests ...
     ```
       Running ‘testthat.R’
      ERROR
     Running the tests in ‘tests/testthat.R’ failed.
     Last 13 lines of output:
         8.         └─DeclareDesign:::future_lapply(...)
         9.           └─base::lapply(...)
        10.             └─DeclareDesign (local) FUN(X[[i]], ...)
        11.               ├─DeclareDesign:::run_design_internal(design)
        12.               └─DeclareDesign:::run_design_internal.design(design)
        13.                 └─DeclareDesign:::next_step(step, current_df, i)
        14.                   └─base::tryCatch(...)
        15.                     └─base (local) tryCatchList(expr, classes, parentenv, handlers)
        16.                       └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        17.                         └─value[[3L]](cond)
       
       [ FAIL 5 | WARN 16 | SKIP 0 | PASS 321 ]
       Error:
       ! Test failures.
       Execution halted
     ```

## In both

*   checking whether package ‘DesignLibrary’ can be installed ... WARNING
     ```
     Found the following significant warnings:
       Warning: package ‘randomizr’ was built under R version 4.6.1
       Warning: package ‘estimatr’ was built under R version 4.6.1
     See ‘/Users/alexandercoppock/git_projects/fabricatr/revdep/checks.noindex/DesignLibrary/new/DesignLibrary.Rcheck/00install.out’ for details.
     ```


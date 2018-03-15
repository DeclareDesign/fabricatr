## Resubmission

This is a resubmission of `fabricatr 0.4.0`, originally submitted yesterday. We had some URLs in documentation that were not yet live, and a case sensitivity issue with a CRAN link in documentation. I fixed these errors. Thank you very much for notifying us of them.

## Test environments
* local macOS install, R 3.4.3
* Windows 3.3, 3.4 on Appveyor
* Ubuntu 14.04, 3.3 and 3.4 on Travis-CI
* macOS 3.3 and 3.4 on Travis-CI
* win-builder (devel and release)
* r-hub (release)

## R CMD check results
No errors, warnings, or notes.

## Downstream dependencies:
Package `estimatr` suggests `fabricatr`. This version should not cause any breaking changes, and I am in contact with Luke Sonnet, package author of `estimatr` to ensure compatibility is maintained going forward.


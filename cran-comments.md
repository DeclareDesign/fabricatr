## Test environments
* local OS X install, R 3.4.2
* OS X install (on travis-ci), R 3.4.2
* OS X install (on travis-ci), R 3.3.3
* ubuntu 14.04 (on travis-ci), R 3.4.2
* ubuntu 14.04 (on travis-ci), R 3.3.3
* windows server 2012 R2 X64 (on appveyor), R 3.3.3
* windows server 2012 R2 X64 (on appveyor), R 3.4.3
* r-hub builder (devel)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There is one NOTE:
> Unknown, possibly mis-spelled, fields in DESCRIPTION:
>  'FasterWith'

As per R-PKGS, "You can also create your own fields to add additional metadata... if you plan to submit to CRAN, the names you use should be valid English words". I think we satisfy those requirements.

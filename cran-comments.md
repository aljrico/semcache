## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* `usearchlite` is listed in Suggests and is not on CRAN. All functionality
  that depends on it is guarded by `requireNamespace()` checks and tests are
  skipped when it is not installed.
* `toonlite` is listed in Suggests and is not on CRAN. Same guards apply.

## Test environments

* macOS (local), R 4.5.1

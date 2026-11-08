## Resubmission

This package was previously archived on CRAN on 2016-02-26 because check
problems were not corrected despite reminders. This submission fixes those
issues:

* Replaced `class(x) != "y"` with `!inherits(x, "y")` in
  `multinom.ci.test()`, which is required since `class()` can return a
  vector for S4/matrix objects (this was producing a WARNING under current
  R versions).
* Regenerated `NAMESPACE` with `roxygen2`, making sure all exports,
  S4 classes and methods, and imports are declared consistently with the
  documented `@export`/`@importFrom` tags in the source.
* Added `\value` documentation to the `show` and `all.equal` methods for
  the `sep.pair` S4 class, which are now explicitly exported.

## Test environments

* local macOS (R 4.6.2, aarch64-apple-darwin23), `R CMD check --as-cran`
* win-builder (R-devel and R-release), via `devtools::check_win_devel()` /
  `devtools::check_win_release()`

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission (resubmission of a previously archived package).
  CRAN automatically reports the archival reason above; no further action
  is required for this note.

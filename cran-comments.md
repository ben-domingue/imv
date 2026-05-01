## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Ubuntu 22.04, R 4.5.3
* win-builder (R-devel)

## Notes

* This is a new submission.
* `lme4` and `mirt` are in `Suggests` and are only loaded when the corresponding
  S3 methods are called. Examples that require these packages are wrapped in
  `\dontrun{}`.

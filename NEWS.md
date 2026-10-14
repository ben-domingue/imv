# imv 0.3.1

## Bug fixes

* `imv.binary()`'s formal arguments are now actually `m0`, `m1`, `p2` as
  documented (and as announced in the 0.3 NEWS); the source still used
  `y`, `p0`, `p1`, which R CMD check flagged as an S3 method/generic
  inconsistency. Positional callers are unaffected.

* Documentation for the `imv` methods now lists the `seed` argument and the
  correct `nfold` default of 5.

* `imv.binary()` now solves for the weighted-coin equivalent of a mean log
  score with a bracketed root finder (`uniroot` on `[0.5, 1]`) instead of
  `nlminb` started at 0.5. The old objective had zero gradient at the start
  and a kink at the root, and `nlminb` occasionally stopped at the wrong
  value without signalling an error (typically ~1e-9 off, in rare cases up
  to ~1e-4). Results for in-domain inputs may change slightly (#14).

* `imv.binary()` now warns (class `imv_out_of_domain`) when a model's mean
  log score is below `-log(2)`, i.e. worse than a fair coin. Previously the
  optimizer silently returned its starting value of 0.5 in this case. The
  returned value is unchanged: the model is still treated as a fair coin.
  Cross-validating `imv()` methods collapse these into a single summary
  warning per model reporting how many folds were affected. This is common
  for prevalence-only baselines on roughly balanced outcomes, whose
  out-of-sample score sits just below the boundary about half the time (#14).

# imv 0.3

## Breaking changes

* `imv.glm.rmvar()` has been renamed to `imvglm.rmvar()`. The old name looked
  like an S3 method for the `imv` generic on a non-existent `glm.rmvar` class.

* `imv.binary()` arguments renamed: `y` → `m0`, `p1` → `m1` for consistency
  with the `imv` S3 generic. Callers using positional arguments are unaffected.

## New features

* `imv()` is now an S3 generic. Methods are available for `glm`, `glmerMod`
  (lme4), and `SingleGroupClass` (mirt) objects. An `imv.default` method
  accepts a custom `predict_fn` for unsupported model types.

## Other changes

* `imv0glm()` and `imvglm.rmvar()` are now exported and documented.
* Added a `testthat` test suite.
* Package licensed under MIT.

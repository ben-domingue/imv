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

# imv 0.4.0

## Breaking changes

* `imv()` for `mirt` models with `m1 = NULL` now compares dichotomous items to
  each item's prevalence in the training fold, as documented. It previously used
  the grand mean of all training responses across items, which favoured the model
  whenever item difficulties differed. Results from `imv()` on a single
  dichotomous `mirt` model will change; two-model comparisons do not. The
  previous baseline is available as `baseline = "grand"` (see
  below), which reproduces earlier results exactly.

## New features

* `imv_c()` and `imv_t()` compute polytomous IMVs. `imv_c()` (category
  discrimination) scores every pair of categories; `imv_t()` (threshold) scores
  every cumulative split. Both are weighted means of binary IMVs and equal
  `imv.binary()` when there are two categories. Their `weights` argument chooses
  the task weights: `"continuity"` (the default, the weights of the working
  note) or the deprecated `"legacy"`, the definitions used before it (cuts
  weighted by `N_k` for `imv_t()`, and the two-stage mean with pair weights
  `N_k + N_k'` for `imv_c()`). A model that predicts worse than a fair coin in
  some tasks gives one warning per call naming those tasks.

* `imv()` for `mirt` models supports polytomous items. `type` (`"c"` or `"t"`)
  chooses the index and `weights` is passed on to it.

* `imv()` for `mirt` models gains `aggregate = c("items", "pooled")` and
  `item.weights = c("responses", "equal")`. `"items"` computes one IMV per item
  and takes their mean, weighted by the number of held-out responses per item
  (`"responses"`, the default) or equally; `"pooled"` makes a single IMV call on
  all held-out responses. The default is `"pooled"` when all items are
  dichotomous (as before) and `"items"` when any item is polytomous. Both work
  for dichotomous and polytomous items. `"pooled"` stops before fitting when
  items have different response categories. Out-of-domain warnings for per-item
  scores name the affected items, sorted naturally (`item2` before `item10`).

* `imv()` for `mirt` models gains `baseline = c("item", "grand")`, the null
  model used when `m1 = NULL`. `"item"` (the default) is each item's training
  prevalence. `"grand"` is one prevalence pooled over all items, the baseline
  for dichotomous models before this version. Use it to reproduce earlier
  results, to measure how much of an earlier IMV came from the baseline, or to
  measure how much item-specific information contributes to prediction. For
  polytomous items it requires every item to share one set of response
  categories.

## Deprecated

* `weights = "legacy"` in `imv_c()`, `imv_t()` and `imv()` is deprecated and
  will be removed in a future release. It warns once per session with a
  condition of class `imv_deprecated`.

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

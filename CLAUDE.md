# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Run all tests
Rscript -e "devtools::test()"

# Run a single test file
Rscript -e "devtools::test(filter='imv-binary')"

# Check the package (full R CMD check)
Rscript -e "devtools::check()"

# Load the package interactively
Rscript -e "devtools::load_all()"

# Rebuild documentation from roxygen comments
Rscript -e "devtools::document()"

# Install package locally
Rscript -e "devtools::install()"
```

## Architecture

`imv` computes the InterModel Vigorish (IMV), a metric for comparing predictive accuracy between two models on binary outcomes. The IMV represents the fraction of cases in which the better model would win bets made using each model's predicted probabilities.

### Dispatch flow

`imv()` is an S3 generic. Calling it on a model object routes to the appropriate method, which performs k-fold cross-validation, then calls `imv.binary()` on each fold's held-out predictions.

```
imv(m0, m1, ...)
  └─► imv.glm() / imv.glmerMod() / imv.SingleGroupClass() / imv.default()
        └─► .cv_fold_imv()       [iterates over folds]
              └─► imv.binary()   [core computation: outcomes + two probability vectors → IMV]
```

- **`imv.binary()`** (`R/imv_binary.R`) — lowest-level function; takes outcome vector and two probability vectors, returns scalar IMV
- **`imv.default()`** (`R/imv.R`) — accepts a user-supplied `predict_fn` for arbitrary model types
- **`.cv_fold_imv()`** and **`.build_imv_result()`** (`R/imv.R`) — shared cross-validation helpers used by all S3 methods
- Legacy functions `imv0glm()` / `imvglm.rmvar()` (`R/imvglm.R`) are exported but superseded by the S3 interface

### S3 methods

| File | Method | Model type |
|------|--------|------------|
| `R/imv_glm_method.R` | `imv.glm` | `stats::glm` (binomial) |
| `R/imv_lmer_method.R` | `imv.glmerMod` | `lme4::glmer` (binomial) |
| `R/imv_mirt.R` | `imv.SingleGroupClass` | `mirt::mirt` IRT models |

The mirt method has additional helpers (`.mirt_long_format()`, `.eval_mirt_call()`, `makeresponse()`) for reshaping item-response data into the binary format expected by `imv.binary()`.

### Optional dependencies

`lme4` and `mirt` are in `Suggests`, not `Imports`. Methods that need them check availability at runtime. Tests that use these packages are wrapped accordingly.

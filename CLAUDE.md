# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

**imv** is an R package implementing the InterModel Vigorish (IMV) metric for comparing the predictive value of two models on binary outcomes. IMV measures the expected profit/loss of a bettor using one model's predictions versus another's.

## Development commands

All development is done through R (no Makefile or shell scripts):

```r
# Load package for interactive development
devtools::load_all()

# Run the test suite
devtools::test()

# Rebuild documentation from roxygen2 comments
roxygen2::roxygenise()

# Full package check (documentation, examples, tests, R CMD check)
devtools::check()
```

Tests live in `tests/testthat/`. Run a single file:
```bash
Rscript -e "testthat::test_file('tests/testthat/test-imv-glm.R')"
```

## Architecture

The package is built around a single S3 generic `imv(m0, m1, ...)` that cross-validates two competing models and returns their IMV difference.

**Core flow:**
1. `R/imv.R` — defines the S3 generic (`imv(m0, m1, ...)`), the default method (uses a caller-supplied `predict_fn`), and shared helpers: `.cv_fold_imv()` (splits data into folds, refits, predicts) and `.build_imv_result()` (aggregates fold results into mean/sd/CI)
2. `R/imv_binary.R` — `imv.binary(m0, m1, p2)`: the atomic calculation. Given a binary outcome vector and two probability vectors, computes the IMV score for that fold. Note: first two args are named `m0`/`m1` (not `y`/`p1`) for S3 consistency, but the function is always called positionally.
3. Model-specific methods in `R/imv_glm_method.R`, `R/imv_lmer_method.R`, `R/imv_mirt.R` — each method extracts the outcome variable and formula from its model object, then delegates to the shared CV infrastructure in `imv.R`

**Return value of `imv()`:** a list with `folds` (per-fold scores), `mean`, `sd`, and `ci` (95% confidence interval).

**Optional dependencies:** `lme4` (for `glmerMod` method) and `mirt` (for `SingleGroupClass` method) are in `Suggests`, not `Imports` — the core package has no external dependencies.

**Legacy API:** `R/imvglm.R` contains `imv0glm()` and `imvglm.rmvar()` — older functions predating the S3 generic, kept for backwards compatibility. Note: the function was previously named `imv.glm.rmvar` (renamed to avoid appearing as an S3 method).

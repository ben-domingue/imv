skip_if_not_installed("workflows")
skip_if_not_installed("parsnip")

set.seed(42)
n  <- 300
x1 <- rnorm(n); x2 <- rnorm(n)
y  <- factor(rbinom(n, 1, plogis(3 * x1)))
df <- data.frame(y = y, x1 = x1, x2 = x2)

spec <- parsnip::logistic_reg() |> parsnip::set_engine("glm")

wf0 <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(y ~ x2)
wf0 <- workflows::fit(wf0, data = df)

wf1 <- workflows::workflow() |>
    workflows::add_model(spec) |>
    workflows::add_formula(y ~ x1 + x2)
wf1 <- workflows::fit(wf1, data = df)

test_that("imv.workflow returns expected list structure", {
    out <- imv(wf0, wf1, data = df, nfold = 3)
    expect_named(out, c("folds", "mean", "sd", "ci"))
    expect_length(out$folds, 3)
    expect_named(out$ci, c("lower", "upper"))
})

test_that("imv.workflow fold values are finite", {
    out <- imv(wf0, wf1, data = df, nfold = 3)
    expect_true(all(is.finite(out$folds)))
})

test_that("imv.workflow: signal predictor gives positive mean IMV", {
    out <- imv(wf0, wf1, data = df, nfold = 4)
    expect_gt(out$mean, 0)
})

test_that("imv.workflow: y auto-extracted from formula", {
    out <- imv(wf0, wf1, data = df, nfold = 3)
    expect_true(is.numeric(out$mean))
})

test_that("imv.workflow errors without data", {
    expect_error(imv(wf0, wf1), regexp = "data")
})

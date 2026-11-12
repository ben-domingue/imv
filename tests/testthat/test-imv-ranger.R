skip_if_not_installed("ranger")

set.seed(42)
n  <- 300
x1 <- rnorm(n); x2 <- rnorm(n)
y  <- factor(rbinom(n, 1, plogis(3 * x1)))
df <- data.frame(y = y, x1 = x1, x2 = x2)

m0 <- ranger::ranger(dependent.variable.name = "y",
                     data = df[, c("y", "x2")],
                     num.trees = 100, probability = TRUE, verbose = FALSE)
m1 <- ranger::ranger(dependent.variable.name = "y",
                     data = df,
                     num.trees = 100, probability = TRUE, verbose = FALSE)

## m0 is a forest on a pure-noise predictor. Out of sample its probabilities are
## miscalibrated enough to score worse than a fair coin in every fold (log score
## about -0.8 to -1.0, at any outcome prevalence or min.node.size tried), so
## imv() warns and treats it as a fair coin. The warning is correct here: the
## tests assert it, then check the result.
imv_noise_baseline <- function(...) {
    out <- NULL
    expect_warning(out <- imv(...), "baseline model's mean log score was below -log\\(2\\)")
    out
}

test_that("imv.ranger returns expected list structure", {
    out <- imv_noise_baseline(m0, m1, data = df, nfold = 3)
    expect_named(out, c("folds", "mean", "sd", "ci"))
    expect_length(out$folds, 3)
    expect_named(out$ci, c("lower", "upper"))
})

test_that("imv.ranger fold values are finite", {
    out <- imv_noise_baseline(m0, m1, data = df, nfold = 3)
    expect_true(all(is.finite(out$folds)))
})

test_that("imv.ranger: signal predictor gives positive mean IMV", {
    out <- imv_noise_baseline(m0, m1, data = df, nfold = 4)
    expect_gt(out$mean, 0)
})

test_that("imv.ranger errors without probability = TRUE", {
    m_reg <- ranger::ranger(dependent.variable.name = "y",
                            data = df, num.trees = 50, verbose = FALSE)
    expect_error(imv(m_reg, m1, data = df), regexp = "probability")
})

test_that("imv.ranger errors without data", {
    expect_error(imv(m0, m1), regexp = "data")
})

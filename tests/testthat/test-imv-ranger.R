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

test_that("imv.ranger returns expected list structure", {
    out <- imv(m0, m1, data = df, nfold = 3)
    expect_named(out, c("folds", "mean", "sd", "ci"))
    expect_length(out$folds, 3)
    expect_named(out$ci, c("lower", "upper"))
})

test_that("imv.ranger fold values are finite", {
    out <- imv(m0, m1, data = df, nfold = 3)
    expect_true(all(is.finite(out$folds)))
})

test_that("imv.ranger: signal predictor gives positive mean IMV", {
    out <- imv(m0, m1, data = df, nfold = 4)
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

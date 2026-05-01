set.seed(5)
n  <- 200
x  <- rnorm(n)
y  <- rbinom(n, 1, plogis(x))
df <- data.frame(x = x, y = y)
m0 <- glm(y ~ 1, df, family = "binomial")
m1 <- glm(y ~ x, df, family = "binomial")

# Wrap in an unknown class to force dispatch to imv.default
m0_d <- m0; class(m0_d) <- "unknown_xyz"
m1_d <- m1; class(m1_d) <- "unknown_xyz"
pfn  <- function(model, newdata) {
    class(model) <- c("glm", "lm")
    predict(model, newdata, type = "response")
}

test_that("predict_fn path returns correct structure", {
    out <- imv(m0_d, m1_d, data = df, y = "y", predict_fn = pfn, nfold = 3)
    expect_named(out, c("folds", "mean", "sd", "ci"))
    expect_length(out$folds, 3)
})

test_that("predict_fn path gives positive IMV for informative model", {
    out <- imv(m0_d, m1_d, data = df, y = "y", predict_fn = pfn, nfold = 4)
    expect_gt(out$mean, 0)
})

test_that("missing predict_fn on unsupported class errors", {
    expect_error(imv(m0_d, m0_d), regexp = "predict_fn")
})

test_that("missing data with predict_fn errors", {
    expect_error(imv(m0_d, m1_d, y = "y", predict_fn = pfn), regexp = "data")
})

test_that("missing y with predict_fn errors", {
    expect_error(imv(m0_d, m1_d, data = df, predict_fn = pfn), regexp = "y")
})

test_that("wrong y column name errors", {
    expect_error(
        imv(m0_d, m1_d, data = df, y = "nonexistent", predict_fn = pfn),
        regexp = "nonexistent"
    )
})

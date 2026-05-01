set.seed(1)
n  <- 500
x  <- rnorm(n)
y  <- rbinom(n, 1, plogis(x))
pr_good <- plogis(x)
pr_null <- rep(mean(y), n)

test_that("imv.binary returns a scalar", {
    out <- imv.binary(y, pr_null, pr_good)
    expect_length(out, 1)
    expect_true(is.numeric(out))
})

test_that("informative predictor gives positive IMV", {
    expect_gt(imv.binary(y, pr_null, pr_good), 0)
})

test_that("null vs null gives IMV near zero", {
    expect_equal(imv.binary(y, pr_null, pr_null), 0, tolerance = 1e-8)
})

test_that("reversed models give negative IMV", {
    expect_lt(imv.binary(y, pr_good, pr_null), 0)
})

test_that("sigma clips extremes without error", {
    p_extreme <- c(0, 1, pr_good[-c(1, 2)])
    expect_no_error(imv.binary(y, pr_null, p_extreme))
})

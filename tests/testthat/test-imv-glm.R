set.seed(42)
n  <- 300
x  <- rnorm(n)
z  <- rnorm(n)
y  <- rbinom(n, 1, plogis(x))
df <- data.frame(x = x, z = z, y = y)
m0 <- glm(y ~ 1,     df, family = "binomial")
m1 <- glm(y ~ x,     df, family = "binomial")
m2 <- glm(y ~ x + z, df, family = "binomial")

test_that("imv() returns expected list structure", {
    out <- imv(m0, m1, nfold = 3)
    expect_named(out, c("folds", "mean", "sd", "ci"))
    expect_length(out$folds, 3)
    expect_length(out$ci, 2)
    expect_named(out$ci, c("lower", "upper"))
})

test_that("informative predictor gives positive mean IMV", {
    out <- imv(m0, m1, nfold = 4)
    expect_gt(out$mean, 0)
})

test_that("noise predictor gives mean IMV near zero", {
    set.seed(7)
    out <- imv(m1, m2, nfold = 4)
    expect_lt(abs(out$mean), 0.05)
})

test_that("ci lower <= mean <= ci upper", {
    out <- imv(m0, m1, nfold = 4)
    expect_lte(out$ci["lower"], out$mean)
    expect_gte(out$ci["upper"], out$mean)
})

test_that("explicit data argument works", {
    out <- imv(m0, m1, data = df, nfold = 3)
    expect_named(out, c("folds", "mean", "sd", "ci"))
})

test_that("too few rows for nfold errors clearly", {
    tiny <- df[1:3, ]
    expect_error(imv(m0, m1, data = tiny, nfold = 4), regexp = "complete")
})

test_that("seed argument produces reproducible results", {
    out1 <- imv(m0, m1, nfold = 4, seed = 7)
    out2 <- imv(m0, m1, nfold = 4, seed = 7)
    expect_equal(out1$folds, out2$folds)
})

test_that("different seeds produce different fold splits", {
    out1 <- imv(m0, m1, nfold = 4, seed = 1)
    out2 <- imv(m0, m1, nfold = 4, seed = 2)
    expect_false(identical(out1$folds, out2$folds))
})

test_that("ci uses t-distribution: width > z-based for small nfold", {
    out <- imv(m0, m1, nfold = 3, seed = 1)
    se  <- out$sd / sqrt(3)
    z_half_width <- 1.96 * se
    t_half_width <- qt(0.975, df = 2) * se
    actual_half  <- unname((out$ci["upper"] - out$ci["lower"]) / 2)
    expect_gt(actual_half, z_half_width)
    expect_equal(actual_half, t_half_width, tolerance = 1e-10)
})

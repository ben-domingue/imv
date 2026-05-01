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

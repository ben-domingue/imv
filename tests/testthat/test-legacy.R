set.seed(99)
n  <- 300
x  <- rnorm(n)
z  <- rnorm(n)
y  <- rbinom(n, 1, plogis(x))
df <- data.frame(x = x, z = z, y = y)
m  <- glm(y ~ x + z, df, family = "binomial")

test_that("imv0glm returns nfold-length numeric vector", {
    out <- imv0glm(m, nfold = 4)
    expect_length(out, 4)
    expect_true(is.numeric(out))
})

test_that("imv0glm: informative model gives positive mean", {
    m_strong <- glm(y ~ x, df, family = "binomial")
    out <- imv0glm(m_strong, nfold = 4)
    expect_gt(mean(out), 0)
})

test_that("imvglm.rmvar returns nfold-length numeric vector", {
    out <- imvglm.rmvar(m, nfold = 4, var.nm = "z")
    expect_length(out, 4)
    expect_true(is.numeric(out))
})

test_that("imvglm.rmvar: removing noise predictor gives IMV near zero", {
    out <- imvglm.rmvar(m, nfold = 4, var.nm = "z")
    expect_lt(abs(mean(out)), 0.05)
})

test_that("imv0glm nfold > nrow errors with message", {
    tiny <- df[1:3, ]
    mt   <- suppressWarnings(glm(y ~ x, tiny, family = "binomial"))
    expect_error(imv0glm(mt, nfold = 10), regexp = "nfold")
})

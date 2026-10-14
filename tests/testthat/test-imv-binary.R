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

## ---- Coin solver: regression tests from external MRE (GitHub issue #14) ----

h_coin <- function(w) if (w >= 1) 0 else w * log(w) + (1 - w) * log1p(-w)
ref_coin <- function(L) uniroot(function(w) h_coin(w) - L, c(0.5, 1),
                                tol = 1e-14)$root

test_that("positive control: known coins 0.75 and 0.80 are recovered exactly", {
    p0 <- exp(h_coin(0.75))
    p1 <- exp(h_coin(0.80))
    expect_equal(imv.binary(1, p0, p1), (0.80 - 0.75) / 0.75, tolerance = 1e-10)
})

test_that("in-domain case where nlminb previously mis-converged matches bracketed root", {
    ## nlminb returned -0.000856 here; the true value is -0.001041
    ref <- ref_coin(log(0.70365)) / ref_coin(log(0.705)) - 1
    expect_equal(imv.binary(1, 0.705, 0.70365), ref, tolerance = 1e-10)
    expect_equal(imv.binary(1, 0.705, 0.70365), -0.0010414807805, tolerance = 1e-8)
})

test_that("solver matches bracketed reference across many in-domain scores", {
    set.seed(3)
    for (i in 1:50) {
        n  <- 200
        xx <- rnorm(n)
        yy <- rbinom(n, 1, plogis(xx))
        pa <- plogis(0.7 * xx)
        pb <- plogis(xx)
        La <- mean(yy * log(pa) + (1 - yy) * log(1 - pa))
        Lb <- mean(yy * log(pb) + (1 - yy) * log(1 - pb))
        skip_if(La < -log(2) || Lb < -log(2))
        expect_equal(imv.binary(yy, pa, pb), ref_coin(Lb) / ref_coin(La) - 1,
                     tolerance = 1e-10)
    }
})

test_that("both models out of domain warns and returns 0", {
    w <- capture_warnings(out <- imv.binary(1, 0.4, 0.45))
    expect_length(w, 2)
    expect_match(w[1], "baseline model")
    expect_match(w[2], "enhanced model")
    expect_equal(out, 0)
})

test_that("baseline out of domain warns and floors baseline at a fair coin", {
    expect_warning(out <- imv.binary(1, 0.4, 0.6), "baseline model")
    expect_equal(out, (ref_coin(log(0.6)) - 0.5) / 0.5, tolerance = 1e-10)
})

test_that("in-domain inputs do not warn", {
    expect_no_warning(imv.binary(y, pr_null, pr_good))
})

test_that("coin solver handles domain endpoints", {
    expect_equal(.imv_coin(0), 1)
    expect_equal(.imv_coin(-log(2)), 0.5)
    expect_equal(.imv_coin(-1e-12), 1, tolerance = 1e-6)
    expect_equal(.imv_coin(-log(2) + 1e-12), 0.5, tolerance = 1e-5)
    expect_true(is.na(.imv_coin(NA)))
})

test_that("out-of-domain warning is classed", {
    expect_warning(imv.binary(1, 0.4, 0.6), class = "imv_out_of_domain")
})

test_that(".tally_out_of_domain collapses per-fold warnings into one per model", {
    w <- capture_warnings(
        .tally_out_of_domain(3, for (i in 1:3) imv.binary(1, 0.4, 0.6))
    )
    expect_length(w, 1)
    expect_match(w, "In 3 of 3 folds the baseline model")
    w <- capture_warnings(
        .tally_out_of_domain(4, for (i in 1:4)
            imv.binary(1, if (i == 1) 0.4 else 0.7, if (i <= 2) 0.45 else 0.7))
    )
    expect_length(w, 2)
    expect_match(w[1], "In 1 of 4 folds the baseline model")
    expect_match(w[2], "In 2 of 4 folds the enhanced model")
    expect_no_warning(.tally_out_of_domain(2, for (i in 1:2) imv.binary(1, 0.7, 0.8)))
})

test_that("imv() with a near-coin baseline warns at most once per model", {
    set.seed(11)
    d  <- data.frame(x = rnorm(400))
    d$y <- rbinom(400, 1, plogis(0.2 * d$x))
    m0 <- glm(y ~ 1, d, family = binomial)
    m1 <- glm(y ~ x, d, family = binomial)
    w <- capture_warnings(imv(m0, m1, nfold = 10, seed = 1))
    expect_lte(length(w), 2)
    if (length(w)) expect_match(w, "of 10 folds", all = TRUE)
})

## graded response item, K = 4 (example from the imv-poly working note)
set.seed(1)
N <- 2000; K <- 4
theta <- rnorm(N); b <- c(-1, 0, 1.2)
cum <- sapply(b, function(bk) plogis(1.5 * (theta - bk)))
P1 <- cbind(1 - cum[, 1], cum[, 1] - cum[, 2], cum[, 2] - cum[, 3], cum[, 3])
x  <- apply(P1, 1, function(p) sample(0:(K - 1), 1, prob = p))
P0 <- tabulate(x + 1, K) / N

test_that("reproduces the working-note example (sigma = 1e-6)", {
    expect_equal(as.numeric(imv_c(x, P0, P1, K, sigma = 1e-6)), 0.3847861, tolerance = 1e-6)
    expect_equal(as.numeric(imv_t(x, P0, P1, K, sigma = 1e-6)), 0.2961391, tolerance = 1e-6)
})

test_that("informative model is positive, identical models are zero", {
    expect_gt(imv_c(x, P0, P1, K), 0)
    expect_gt(imv_t(x, P0, P1, K), 0)
    expect_equal(as.numeric(imv_c(x, P1, P1, K)), 0, tolerance = 1e-10)
    expect_equal(as.numeric(imv_t(x, P1, P1, K)), 0, tolerance = 1e-10)
})

test_that("K = 2 reduces to imv.binary", {
    y  <- as.integer(x >= 2)
    Q1 <- cbind(1 - cum[, 2], cum[, 2]); q0 <- c(1 - mean(y), mean(y))
    ref <- imv.binary(as.numeric(y == 0), q0[1], Q1[, 1])
    expect_equal(as.numeric(imv_c(y, q0, Q1, 2)), ref, tolerance = 1e-10)
    expect_equal(as.numeric(imv_t(y, q0, Q1, 2)), ref, tolerance = 1e-10)
})

test_that("omega_t is invariant to reversing category order", {
    xr <- (K - 1L) - x
    expect_equal(as.numeric(imv_t(xr, rev(P0), P1[, K:1], K)),
                 as.numeric(imv_t(x, P0, P1, K)), tolerance = 1e-10)
})

test_that("an empty category is handled (zero weight, defined result)", {
    keep <- x != 3
    oc <- imv_c(x[keep], P0, P1[keep, ], K)
    ot <- imv_t(x[keep], P0, P1[keep, ], K)
    expect_true(is.finite(oc)); expect_true(is.finite(ot))
    expect_true(all(is.na(attr(ot, "components")$omega[3])))
})

test_that("bad coding is rejected", {
    expect_error(imv_c(x + 1L, P0, P1, K), "coded 0")
    expect_error(imv_t(x, P0[-1], P1, K), "length K")
})

test_that("omega_t weights do not overflow for large N", {
    ## n_lo * (N - n_lo) exceeds .Machine$integer.max once N > 46340
    set.seed(2)
    Nb <- 60000
    th <- rnorm(Nb)
    cb <- sapply(b, function(bk) plogis(1.5 * (th - bk)))
    Pb <- cbind(1 - cb[, 1], cb[, 1] - cb[, 2], cb[, 2] - cb[, 3], cb[, 3])
    xb <- rowSums(matrix(runif(Nb), Nb, K - 1) < cb)
    expect_no_warning(ot <- imv_t(xb, tabulate(xb + 1, K) / Nb, Pb, K))
    w <- attr(ot, "components")$weight
    expect_true(is.finite(ot))
    expect_true(all(is.finite(w) & w > 0))
})

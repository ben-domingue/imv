## The lab's imvhelper.R, the definitions used before the working note. Renamed
## so they do not mask the package's imv, imv_c and imv_t; the print() calls are
## removed. Nothing else is changed.
.legacy_imv <- function(pr, p1, p2, eps = 1e-6) {
  pr$pv1 <- pr[[p1]]
  pr$pv2 <- pr[[p2]]
  pr$pv1 <- ifelse(pr$pv1 < eps, eps, pr$pv1)
  pr$pv2 <- ifelse(pr$pv2 < eps, eps, pr$pv2)
  pr$pv1 <- ifelse(pr$pv1 > 1 - eps, 1 - eps, pr$pv1)
  pr$pv2 <- ifelse(pr$pv2 > 1 - eps, 1 - eps, pr$pv2)
  ##
  ll <- function(x, p = 'pv') {
    z <- log(x[[p]]) * x$resp + log(1 - x[[p]]) * (1 - x$resp)
    z <- sum(z) / nrow(x)
    exp(z)
  }
  loglik1 <- ll(pr, 'pv1')
  loglik2 <- ll(pr, 'pv2')
  getcoins <- function(a) {
    f <- function(p, a) abs(p * log(p) + (1 - p) * log(1 - p) - log(a))
    nlminb(.5, f, lower = 0.001, upper = .999, a = a)$par
  }
  c1 <- getcoins(loglik1)
  c2 <- getcoins(loglik2)
  ew <- function(p1, p0) (p1 - p0) / p0
  imv <- ew(c2, c1)
  return(imv)
}

.legacy_imv_c <- function(y, pctt.tab, p1, p2) {
  nn <- length(unique(y$resp))
  om <- numeric()
  iis <- 0:(nn - 1)
  for (ii in iis) {
    ns <- om.tmp <- numeric()
    jjs <- iis[-match(ii, iis)]
    for (jj in jjs) {
      y2 <- y[y$resp %in% c(ii, jj), ]
      resp <- ifelse(y2$resp == ii, 1, 0)
      ##irt p-values for being
      p1.ii <- y2[[paste(p1, ii, sep = '')]]
      p1.jj <- y2[[paste(p1, jj, sep = '')]]
      p2.ii <- y2[[paste(p2, ii, sep = '')]]
      p2.jj <- y2[[paste(p2, jj, sep = '')]]
      ##
      z <- data.frame(
        resp = resp,
        p1 = p1.ii / (p1.ii + p1.jj),
        p2 = p2.ii / (p2.ii + p2.jj)
      )
      j0 <- as.character(jj)
      om.tmp[j0] <- .legacy_imv(z, p1 = "p1", p2 = "p2")
      ns[as.character(jj)] <- nrow(z)
    }
    om[ii + 1] <- sum(om.tmp * ns) / sum(ns)
  }
  omega_c <- sum(om * pctt.tab) / sum(pctt.tab)
  return(omega_c)
}

.legacy_imv_t <- function(y, pctt.tab, p1, p2) {
  nn <- length(unique(y$resp))
  om <- numeric()
  for (ii in 0:(nn - 2)) {
    resp <- ifelse(y$resp <= ii, 1, 0)
    ##irt p-values for being below ii
    pr1 <- rowSums(y[, paste(p1, 0:ii, sep = ''), drop = FALSE])
    pr2 <- rowSums(y[, paste(p2, 0:ii, sep = ''), drop = FALSE])
    z <- data.frame(resp = resp, p1 = pr1, p2 = pr2)
    om[ii + 1] <- .legacy_imv(z, p1 = "p1", p2 = "p2")
  }
  ##
  pctt.tab <- pctt.tab[1:(nn - 1)] / (1 - pctt.tab[nn])
  ##
  omega_t <- sum(om * pctt.tab) / sum(pctt.tab)
  return(omega_t)
}

## Legacy weights warn once per session; tests that are not about the warning
## call the functions through this wrapper.
legacy <- function(f, ...) {
    withCallingHandlers(f(..., weights = "legacy"),
                        imv_deprecated = function(w) invokeRestart("muffleWarning"))
}
reset_legacy_warning <- function() assign("legacy_warned", FALSE, envir = .imv_state)

## graded response item, K = 4
set.seed(42)
N <- 500; K <- 4
theta <- rnorm(N); b <- c(-0.8, 0.2, 1.1)
cum <- sapply(b, function(bk) plogis(1.4 * (theta - bk)))
P1 <- cbind(1 - cum[, 1], cum[, 1] - cum[, 2], cum[, 2] - cum[, 3], cum[, 3])
x  <- apply(P1, 1, function(p) sample(0:(K - 1), 1, prob = p))
P0 <- tabulate(x + 1, K) / N

## Direct two-stage omega_c: within category k, pairs weighted by N_k + N_k';
## then categories weighted by N_k. Pairs with an undefined task are dropped.
two_stage_c <- function(x, P0, P1, K, sigma) {
    comp <- attr(imv_c(x, P0, P1, K, sigma = sigma), "components")
    Nk   <- tabulate(x + 1, K)
    om_k <- vapply(0:(K - 1), function(k) {
        on <- (comp$k == k | comp$kp == k) & !is.na(comp$omega)
        w  <- Nk[comp$k[on] + 1] + Nk[comp$kp[on] + 1]
        if (!any(on)) NA_real_ else sum(w * comp$omega[on]) / sum(w)
    }, 0)
    ok <- !is.na(om_k)
    sum(Nk[ok] * om_k[ok]) / sum(Nk[ok])
}

test_that("legacy omega_c equals the direct two-stage mean", {
    expect_equal(as.numeric(legacy(imv_c, x, P0, P1, K)), two_stage_c(x, P0, P1, K, 1e-4),
                 tolerance = 1e-12)
    keep <- x != 2                                   # an empty middle category
    expect_equal(as.numeric(legacy(imv_c, x[keep], P0, P1[keep, ], K)),
                 two_stage_c(x[keep], P0, P1[keep, ], K, 1e-4), tolerance = 1e-12)
})

test_that("legacy omega_t is the N_k-weighted mean of the cut IMVs", {
    ot <- legacy(imv_t, x, P0, P1, K)
    cp <- attr(ot, "components")
    Nk <- tabulate(x + 1, K)
    expect_equal(cp$weight, as.numeric(Nk[-K]))
    expect_equal(as.numeric(ot), sum(Nk[-K] * cp$omega) / sum(Nk[-K]), tolerance = 1e-12)
})

test_that("legacy weights reproduce imvhelper.R on a fixed seed", {
    y <- data.frame(resp = x,
                    setNames(as.data.frame(matrix(P0, N, K, byrow = TRUE)), paste0("p0_", 0:3)),
                    setNames(as.data.frame(P1), paste0("p1_", 0:3)))
    tab <- tabulate(x + 1, K) / N
    pinned_c <- 0.33680394353509191
    pinned_t <- 0.24709050657834236
    ## the helper solves with nlminb, whose last digits vary across machines
    expect_equal(.legacy_imv_c(y, tab, "p0_", "p1_"), pinned_c, tolerance = 1e-8)
    expect_equal(.legacy_imv_t(y, tab, "p0_", "p1_"), pinned_t, tolerance = 1e-8)
    ## the helper clips at 1e-6 and solves with nlminb
    expect_equal(as.numeric(legacy(imv_c, x, P0, P1, K, sigma = 1e-6)), pinned_c, tolerance = 1e-6)
    expect_equal(as.numeric(legacy(imv_t, x, P0, P1, K, sigma = 1e-6)), pinned_t, tolerance = 1e-6)
})

test_that("legacy weights warn once per session with a classed condition", {
    reset_legacy_warning()
    expect_warning(imv_c(x, P0, P1, K, weights = "legacy"), class = "imv_deprecated")
    expect_no_warning(imv_t(x, P0, P1, K, weights = "legacy"))
    reset_legacy_warning()
    expect_warning(imv_t(x, P0, P1, K, weights = "legacy"), "deprecated", class = "imv_deprecated")
})

test_that("continuity weights are the default and silent", {
    reset_legacy_warning()
    expect_no_warning(oc <- imv_c(x, P0, P1, K, weights = "continuity"))
    expect_no_warning(ot <- imv_t(x, P0, P1, K, weights = "continuity"))
    expect_identical(oc, imv_c(x, P0, P1, K))
    expect_identical(ot, imv_t(x, P0, P1, K))
    expect_false(isTRUE(.imv_state$legacy_warned))
})

test_that("legacy and continuity differ, and bad weights are rejected", {
    expect_false(isTRUE(all.equal(as.numeric(legacy(imv_t, x, P0, P1, K)),
                                  as.numeric(imv_t(x, P0, P1, K)))))
    expect_error(imv_c(x, P0, P1, K, weights = "pairs"), "should be one of")
})

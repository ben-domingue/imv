## Argument names follow the imv() generic (m0, m1, ...) for S3 consistency:
## m0 is the binary 0/1 outcome vector, m1 the baseline predicted
## probabilities, p2 the enhanced predicted probabilities. Internal callers
## pass these positionally.
imv.binary <- function(m0, m1, p2,
                       sigma = 1e-4,  # clips extremes to avoid log(0)
                       ...) {
    y  <- m0
    p0 <- pmax(pmin(m1, 1 - sigma), sigma)
    p1 <- pmax(pmin(p2, 1 - sigma), sigma)
    ## mean log score (log of the geometric-mean likelihood)
    ll <- function(x, p) mean(log(p) * x + log(1 - p) * (1 - x))
    c0 <- .imv_coin(ll(y, p0), label = "baseline")
    c1 <- .imv_coin(ll(y, p1), label = "enhanced")
    (c1 - c0) / c0
}

## Weighted-coin equivalent of a mean log score L: solve
##   h(w) = w log(w) + (1 - w) log(1 - w) = L   for w in [0.5, 1].
## h is strictly increasing on [0.5, 1], from -log(2) to 0, so uniroot on
## that bracket is guaranteed to converge. For L < -log(2) (the model scores
## worse than a fair coin) no weighted coin exists; the coin is floored at
## 0.5 with a warning.
.imv_coin <- function(L, label = "model", tol = 1e-12) {
    if (is.na(L)) return(NA_real_)
    if (L >= 0) return(1)
    if (L <= -log(2)) {
        if (L < -log(2)) .imv_out_of_domain_warning(label, L)
        return(0.5)
    }
    h <- function(w) w * log(w) + (1 - w) * log1p(-w) - L
    uniroot(h, lower = 0.5, upper = 1, tol = tol,
            f.lower = -log(2) - L, f.upper = -L)$root
}

## Classed warning so that cross-validation loops can tally these per call
## (see .tally_out_of_domain) instead of warning once per model per fold.
.imv_out_of_domain_warning <- function(label, L) {
    msg <- sprintf(paste0(
        "Mean log score of the %s model (%.4f) is below -log(2); the model ",
        "predicts worse than a fair coin and has no weighted-coin ",
        "equivalent. Treating it as a fair coin (w = 0.5)."), label, L)
    w <- structure(
        list(message = msg, call = NULL, label = label, L = L),
        class = c("imv_out_of_domain", "warning", "condition"))
    warning(w)
}

## Evaluate `expr` (a fold loop calling imv.binary), muffling per-fold
## out-of-domain warnings, then emit at most one summary warning per model.
.tally_out_of_domain <- function(nfold, expr) {
    tally <- list()
    res <- withCallingHandlers(expr, imv_out_of_domain = function(w) {
        tally[[w$label]] <<- c(tally[[w$label]], w$L)
        invokeRestart("muffleWarning")
    })
    for (label in names(tally)) {
        Ls <- tally[[label]]
        warning(sprintf(paste0(
            "In %d of %d folds the %s model's mean log score was below ",
            "-log(2) (range %.4f to %.4f): it predicts worse than a fair ",
            "coin and has no weighted-coin equivalent. Treated as a fair ",
            "coin (w = 0.5) in those folds."),
            length(Ls), nfold, label, min(Ls), max(Ls)), call. = FALSE)
    }
    invisible(res)
}

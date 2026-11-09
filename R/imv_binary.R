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

## Evaluate `expr`, muffling out-of-domain warnings, then summarise them.
## n and unit describe what is being counted, e.g. 5 "folds" or 6 "category
## pairs". Untagged conditions (a fold loop calling imv.binary once per fold)
## give one summary per model; each counts as one fold. Conditions tagged by
## .tag_out_of_domain come from polytomous tasks, several of which can occur in
## one fold, so they are counted by distinct fold or task and reported in one
## summary per call.
.tally_out_of_domain <- function(n, expr, unit = "folds") {
    tally <- list()
    res <- withCallingHandlers(expr, imv_out_of_domain = function(w) {
        tally[[length(tally) + 1L]] <<- w
        invokeRestart("muffleWarning")
    })
    if (length(tally) == 0L) return(invisible(res))
    if (!is.null(tally[[1L]]$task)) {
        warning(.task_out_of_domain_message(tally, n, unit), call. = FALSE)
        return(invisible(res))
    }
    labels <- vapply(tally, function(w) w$label, "")
    Ls     <- vapply(tally, function(w) w$L, 0)
    for (label in unique(labels)) {
        L <- Ls[labels == label]
        warning(sprintf(paste0(
            "In %d of %d folds the %s model's mean log score was below ",
            "-log(2) (range %.4f to %.4f): it predicts worse than a fair ",
            "coin and has no weighted-coin equivalent. Treated as a fair ",
            "coin (w = 0.5) in those folds."),
            length(L), n, label, min(L), max(L)), call. = FALSE)
    }
    invisible(res)
}

## Evaluate `expr`, adding fields such as task, item or fold to any
## out-of-domain warning that passes through, then re-signal it.
.tag_out_of_domain <- function(expr, ...) {
    tags <- list(...)
    withCallingHandlers(expr, imv_out_of_domain = function(w) {
        w[names(tags)] <- tags
        warning(w)
        invokeRestart("muffleWarning")
    })
}

## One message covering every model, naming the affected binary tasks.
.task_out_of_domain_message <- function(tally, n, unit, max_tasks = 20L) {
    labels <- vapply(tally, function(w) w$label, "")
    Ls     <- vapply(tally, function(w) w$L, 0)
    tasks  <- vapply(tally, function(w) {
        if (is.null(w$item)) w$task else paste0(w$item, " [", w$task, "]")
    }, "")
    folds  <- vapply(tally, function(w) if (is.null(w$fold)) NA_integer_ else as.integer(w$fold), 0L)
    parts <- vapply(unique(labels), function(label) {
        on    <- labels == label
        named <- unique(tasks[on])
        count <- if (anyNA(folds[on])) length(named) else length(unique(folds[on]))
        shown <- paste(utils::head(named, max_tasks), collapse = ", ")
        if (length(named) > max_tasks) {
            shown <- paste0(shown, ", and ", length(named) - max_tasks, " more")
        }
        sprintf("%s model: %d of %d %s (range %.4f to %.4f), tasks: %s.",
                label, count, n, unit, min(Ls[on]), max(Ls[on]), shown)
    }, "")
    paste0("Mean log score below -log(2) in some binary tasks: the model ",
           "predicts worse than a fair coin there and has no weighted-coin ",
           "equivalent. Treated as a fair coin (w = 0.5) in those tasks. ",
           paste(parts, collapse = " "))
}

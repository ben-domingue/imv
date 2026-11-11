sim_bin <- function(N = 400, J = 6, seed = 21) {
    set.seed(seed)
    theta <- rnorm(N)
    resp <- sapply(seq_len(J), function(j) rbinom(N, 1, plogis(1.3 * (theta - rnorm(1)))))
    colnames(resp) <- paste0("item", seq_len(J))
    as.data.frame(resp)
}
fit_bin <- function(d, ...) suppressMessages(mirt::mirt(d, 1, verbose = FALSE, ...))
run_bin <- function(...) suppressMessages(suppressWarnings(imv(...)))

## Fold results recorded on a85a4ec, before aggregate was added: the default
## for dichotomous models must stay numerically the same.
pinned_null <- c(0.46934087806129299, 0.58809353370605599, 0.55179729484283802,
                 0.39590393565133158)
pinned_two  <- c(-0.01471073966535362, -0.0084111288572355503, -0.0098898664508344572,
                 -0.0047700920117877722)

test_that("dichotomous mirt results match the values recorded before aggregate", {
    skip_if_not_installed("mirt")
    d  <- sim_bin()
    m0 <- fit_bin(d, itemtype = "Rasch")
    m1 <- fit_bin(d, itemtype = "2PL")
    expect_equal(run_bin(m1, nfold = 4, seed = 7)$folds, pinned_null, tolerance = 1e-12)
    expect_equal(run_bin(m0, m1, nfold = 4, seed = 8)$folds, pinned_two, tolerance = 1e-12)
})

sim_graded <- function(N = 400, J = 5, K = 3, seed = 11) {
    set.seed(seed)
    theta <- rnorm(N)
    resp <- sapply(seq_len(J), function(j) {
        b   <- sort(rnorm(K - 1))
        cum <- sapply(b, function(bk) plogis(1.8 * (theta - bk)))
        rowSums(matrix(runif(N), N, K - 1) < cum)
    })
    colnames(resp) <- paste0("item", seq_len(J))
    as.data.frame(resp)
}
fit_poly <- function(d) suppressMessages(mirt::mirt(d, 1, itemtype = "graded", verbose = FALSE))
quiet_legacy <- function(expr) {
    withCallingHandlers(expr, imv_deprecated = function(w) invokeRestart("muffleWarning"))
}

test_that("pooled equals one imv_c/imv_t call on the stacked held-out cells", {
    skip_if_not_installed("mirt")
    m    <- fit_poly(sim_graded())
    orig <- .mirt_poly_score
    for (type in c("c", "t")) {
        seen <- list()
        local_mocked_bindings(.mirt_poly_score = function(cells, ...) {
            seen[[length(seen) + 1L]] <<- cells
            orig(cells, ...)
        })
        res <- run_bin(m, nfold = 3, seed = 1, type = type, aggregate = "pooled")
        f   <- if (type == "c") imv_c else imv_t
        for (i in seq_along(seen)) {
            cl <- seen[[i]]
            direct <- f(unlist(lapply(cl, `[[`, "x")), do.call(rbind, lapply(cl, `[[`, "P0")),
                        do.call(rbind, lapply(cl, `[[`, "P1")), K = 3)
            expect_equal(res$folds[i], as.numeric(direct), tolerance = 1e-12)
        }
        expect_length(seen, 3)
    }
})

test_that("pooled errors before fitting on mixed K; items runs", {
    skip_if_not_installed("mirt")
    d <- sim_graded()
    d$item1 <- as.integer(d$item1 > 0)
    m <- fit_poly(d)
    local({
        local_mocked_bindings(.eval_mirt_call = function(...) stop("a model was fitted"))
        expect_error(imv(m, nfold = 3, seed = 2, aggregate = "pooled"),
                     "item1 \\(K = 2\\), item2 \\(K = 3\\).*aggregate = \"items\"")
    })
    expect_true(is.finite(run_bin(m, nfold = 3, seed = 2, aggregate = "items")$mean))
})

test_that("aggregate = NULL resolves to pooled for binary and items for polytomous", {
    skip_if_not_installed("mirt")
    mb <- fit_bin(sim_bin(), itemtype = "2PL")
    expect_identical(run_bin(mb, nfold = 4, seed = 3),
                     run_bin(mb, nfold = 4, seed = 3, aggregate = "pooled"))
    mp <- fit_poly(sim_graded())
    expect_identical(run_bin(mp, nfold = 3, seed = 3),
                     run_bin(mp, nfold = 3, seed = 3, aggregate = "items"))
    expect_false(identical(run_bin(mb, nfold = 4, seed = 3)$folds,
                           run_bin(mb, nfold = 4, seed = 3, aggregate = "items")$folds))
})

test_that("item.weights responses and equal differ when one item is mostly missing", {
    skip_if_not_installed("mirt")
    set.seed(4)
    dp <- sim_graded(N = 600)
    dp$item1[sample(600, 480)] <- NA
    mp <- fit_poly(dp)
    db <- sim_bin(N = 600)
    db$item1[sample(600, 480)] <- NA
    mb <- fit_bin(db, itemtype = "2PL")
    for (m in list(mp, mb)) {
        r <- run_bin(m, nfold = 3, seed = 5, aggregate = "items", item.weights = "responses")
        e <- run_bin(m, nfold = 3, seed = 5, aggregate = "items", item.weights = "equal")
        expect_true(all(is.finite(c(r$folds, e$folds))))
        expect_false(isTRUE(all.equal(r$folds, e$folds)))
    }
})

test_that("weights pass through to the polytomous IMVs", {
    skip_if_not_installed("mirt")
    m <- fit_poly(sim_graded())
    for (agg in c("items", "pooled")) {
        cont <- run_bin(m, nfold = 3, seed = 6, type = "t", aggregate = agg)
        leg  <- quiet_legacy(run_bin(m, nfold = 3, seed = 6, type = "t", aggregate = agg,
                                     weights = "legacy"))
        expect_false(isTRUE(all.equal(cont$folds, leg$folds)))
    }
})

test_that("per-item binary warnings give one summary naming the items in order", {
    bad <- function(L) .imv_out_of_domain_warning("baseline", L)
    w <- testthat::capture_warnings(.tally_out_of_domain(4, for (f in 1:2) {
        for (it in c("item10", "item2")) .tag_out_of_domain(bad(-1), item = it, fold = f)
    }))
    expect_length(w, 1)
    expect_match(w, "for some items")
    expect_match(w, "baseline model: 2 of 4 folds", fixed = TRUE)
    expect_match(w, "items: item2, item10.", fixed = TRUE)
    expect_no_match(w, "[", fixed = TRUE)
})

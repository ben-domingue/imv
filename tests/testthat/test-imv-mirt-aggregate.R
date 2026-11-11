sim_bin <- function(N = 400, J = 6, seed = 21) {
    set.seed(seed)
    theta <- rnorm(N)
    resp <- sapply(seq_len(J), function(j) rbinom(N, 1, plogis(1.3 * (theta - rnorm(1)))))
    colnames(resp) <- paste0("item", seq_len(J))
    as.data.frame(resp)
}
fit_bin <- function(d, ...) suppressMessages(mirt::mirt(d, 1, verbose = FALSE, ...))
run_bin <- function(...) suppressMessages(suppressWarnings(imv(...)))

## No test here pins values from a mirt fit: fits differ across platforms and
## BLAS builds (by up to 1e-3), so each test compares imv() with a hand
## computation from the same run's refits, or with another imv() call.

## Record every fold's training responses in long format (the input to
## makeresponse(), before it drops anything) and every refit model with the
## person ids of its rows (the input to .mirt_get_preds()).
record_folds <- function(env = parent.frame()) {
    orig_mr <- makeresponse
    orig_gp <- .mirt_get_preds
    rec <- new.env()
    rec$train <- list()
    rec$fits  <- list()
    local_mocked_bindings(
        makeresponse = function(x, ...) {
            rec$train[[length(rec$train) + 1L]] <- x
            orig_mr(x, ...)
        },
        .mirt_get_preds = function(mm, train_id, ...) {
            rec$fits[[length(rec$fits) + 1L]] <- list(mm = mm, train_id = train_id)
            orig_gp(mm, train_id, ...)
        }, .env = env)
    rec
}

## One dichotomous fold by hand, as defined on a85a4ec: the held-out cells are
## the responses not used for training, kept when their person is in the refit;
## each model predicts a cell by probtrace() at the person's EAP score; p0 comes
## from fit0 or is given (a baseline prevalence).
hand_fold <- function(m, train_long, fit1, fit0 = NULL, p0 = NULL) {
    long <- .mirt_long_format(m)
    key  <- function(d) paste(d$id, d$item)
    ho   <- long[!key(long) %in% key(train_long), ]
    ho   <- ho[ho$id %in% fit1$train_id, ]
    pred <- function(f) {
        th  <- mirt::fscores(f$mm, method = "EAP")
        r   <- match(ho$id, f$train_id)
        out <- numeric(nrow(ho))
        for (it in unique(ho$item)) {
            s <- ho$item == it
            out[s] <- mirt::probtrace(mirt::extract.item(f$mm, it), th[r[s], , drop = FALSE])[, 2]
        }
        out
    }
    if (!is.null(fit0)) p0 <- pred(fit0)
    ## a near-0.5 baseline can score just below -log(2), as it does inside imv()
    withCallingHandlers(imv.binary(ho$resp, p0, pred(fit1)),
                        imv_out_of_domain = function(w) invokeRestart("muffleWarning"))
}

test_that("two-model dichotomous folds equal a hand computation from the refits", {
    skip_if_not_installed("mirt")
    d  <- sim_bin()
    m0 <- fit_bin(d, itemtype = "Rasch")
    m1 <- fit_bin(d, itemtype = "2PL")
    rec <- record_folds()
    res <- run_bin(m0, m1, nfold = 4, seed = 8)
    ## whole.matrix retries also call makeresponse(); the fold loop's calls are last
    train <- utils::tail(rec$train, 4)
    expect_length(rec$fits, 8)
    for (i in 1:4) {
        expect_equal(res$folds[i],
                     hand_fold(m1, train[[i]], fit1 = rec$fits[[2 * i]],
                               fit0 = rec$fits[[2 * i - 1]]),
                     tolerance = 1e-10)
    }
    expect_identical(res, run_bin(m0, m1, nfold = 4, seed = 8, aggregate = "pooled"))
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

## Record each fold's training matrix (the data every model is refit on); its
## column means are the items' training-fold prevalences.
record_training <- function(env = parent.frame()) {
    orig <- .eval_mirt_call
    trains <- list()
    local_mocked_bindings(.eval_mirt_call = function(call_expr, train, caller_env) {
        trains[[length(trains) + 1L]] <<- train
        orig(call_expr, train, caller_env)
    }, .env = env)
    function() trains
}

test_that("the m1 = NULL baseline is each item's training-fold prevalence", {
    skip_if_not_installed("mirt")
    m <- fit_bin(sim_bin(), itemtype = "2PL")
    for (agg in c("pooled", "items")) local({
        trains <- record_training()
        orig   <- .mirt_binary_score
        seen   <- list()
        local_mocked_bindings(.mirt_binary_score = function(resp, p0, p1, item, ...) {
            seen[[length(seen) + 1L]] <<- list(resp = resp, p0 = p0, p1 = p1, item = item)
            orig(resp, p0, p1, item, ...)
        })
        res <- run_bin(m, nfold = 4, seed = 9, aggregate = agg)
        expect_length(seen, 4)
        for (i in seq_along(seen)) {
            prev <- colMeans(trains()[[i]], na.rm = TRUE)
            expect_equal(seen[[i]]$p0, unname(prev[seen[[i]]$item]), tolerance = 1e-12)
            if (agg == "pooled") {
                expect_equal(res$folds[i], imv.binary(seen[[i]]$resp, seen[[i]]$p0, seen[[i]]$p1),
                             tolerance = 1e-12)
            }
        }
    })
})

test_that("dichotomous items in a mixed model use the same item baseline", {
    skip_if_not_installed("mirt")
    d <- sim_graded()
    d$item1 <- as.integer(d$item1 > 0)
    m <- fit_poly(d)
    trains <- record_training()
    orig   <- .mirt_poly_score
    seen   <- list()
    local_mocked_bindings(.mirt_poly_score = function(cells, ...) {
        seen[[length(seen) + 1L]] <<- cells
        orig(cells, ...)
    })
    run_bin(m, nfold = 3, seed = 10)
    for (i in seq_along(seen)) {
        prev <- mean(trains()[[i]][, "item1"], na.rm = TRUE)
        expect_equal(unique(seen[[i]]$item1$P0[, 2]), prev, tolerance = 1e-12)
    }
})

test_that('baseline = "grand" equals the a85a4ec computation by hand', {
    skip_if_not_installed("mirt")
    m1  <- fit_bin(sim_bin(), itemtype = "2PL")
    rec <- record_folds()
    res <- run_bin(m1, nfold = 4, seed = 7, baseline = "grand")
    expect_length(rec$train, 4)
    for (i in 1:4) {
        ## a85a4ec: p0 = mean(x$resp[x$group != i]), every training response
        ## of every item, including persons makeresponse() later drops
        grand <- mean(rec$train[[i]]$resp)
        expect_equal(res$folds[i], hand_fold(m1, rec$train[[i]], fit1 = rec$fits[[i]], p0 = grand),
                     tolerance = 1e-10)
    }
    expect_identical(run_bin(m1, nfold = 4, seed = 7, baseline = "item"),
                     run_bin(m1, nfold = 4, seed = 7))
})

test_that("baseline has no effect when m1 is supplied", {
    skip_if_not_installed("mirt")
    d  <- sim_bin()
    m0 <- fit_bin(d, itemtype = "Rasch")
    m1 <- fit_bin(d, itemtype = "2PL")
    expect_identical(run_bin(m0, m1, nfold = 4, seed = 8, baseline = "grand"),
                     run_bin(m0, m1, nfold = 4, seed = 8, baseline = "item"))
})

test_that("item and grand baselines agree for equal difficulties and not for varied ones", {
    skip_if_not_installed("mirt")
    sim_b <- function(b, N = 1000, seed = 12) {
        set.seed(seed)
        theta <- rnorm(N)
        d <- as.data.frame(sapply(b, function(bj) rbinom(N, 1, plogis(1.3 * (theta - bj)))))
        names(d) <- paste0("item", seq_along(b))
        d
    }
    both <- function(d) {
        m <- fit_bin(d, itemtype = "2PL")
        c(item  = run_bin(m, nfold = 4, seed = 13, baseline = "item")$mean,
          grand = run_bin(m, nfold = 4, seed = 13, baseline = "grand")$mean)
    }
    ## Equal difficulties: across data seeds 1-12, grand - item ranged from -0.027
    ## to 0.014 with either sign, the size of the fold-to-fold sd (0.01 to 0.05).
    ## Varied difficulties: grand exceeds item by about 0.58 (0.61 vs 0.03).
    same <- both(sim_b(rep(0, 6)))
    expect_lt(abs(same[["item"]] - same[["grand"]]), 0.03)
    varied <- both(sim_b(seq(-2, 2, length.out = 6)))
    expect_gt(varied[["grand"]] - varied[["item"]], 0.3)
})

test_that('baseline = "grand" pools category proportions when items share a format', {
    skip_if_not_installed("mirt")
    m <- fit_poly(sim_graded())
    trains <- record_training()
    orig   <- .mirt_poly_score
    seen   <- list()
    local_mocked_bindings(.mirt_poly_score = function(cells, ...) {
        seen[[length(seen) + 1L]] <<- cells
        orig(cells, ...)
    })
    res <- run_bin(m, nfold = 3, seed = 14, baseline = "grand")
    expect_length(seen, 3)
    for (i in seq_along(seen)) {
        tr     <- unlist(trains()[[i]])
        pooled <- tabulate(tr[!is.na(tr)] + 1L, 3) / sum(!is.na(tr))
        for (cl in seen[[i]]) {
            expect_equal(unname(cl$P0), matrix(pooled, nrow(cl$P0), 3, byrow = TRUE),
                         tolerance = 1e-12)
        }
    }
    expect_false(isTRUE(all.equal(res$folds, run_bin(m, nfold = 3, seed = 14)$folds)))
})

test_that('baseline = "grand" errors before fitting when category sets differ', {
    skip_if_not_installed("mirt")
    d <- sim_graded()
    d$item1 <- as.integer(d$item1 > 0)
    m <- fit_poly(d)
    local({
        local_mocked_bindings(.eval_mirt_call = function(...) stop("a model was fitted"))
        expect_error(imv(m, nfold = 3, seed = 15, baseline = "grand"),
                     'baseline = "grand".*item1 \\(K = 2\\).*baseline = "item"')
    })
    m0 <- suppressMessages(mirt::mirt(d, 1, itemtype = "Rasch", verbose = FALSE))
    expect_true(is.finite(run_bin(m0, m, nfold = 3, seed = 15, baseline = "grand")$mean))
})

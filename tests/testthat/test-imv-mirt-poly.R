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
fit <- function(d, ...) suppressMessages(mirt::mirt(d, 1, verbose = FALSE, ...))
run <- function(...) suppressMessages(suppressWarnings(imv(...)))

test_that("graded models beat the prevalence baseline", {
    skip_if_not_installed("mirt")
    m <- fit(sim_graded(), itemtype = "graded")
    for (type in c("c", "t")) {
        res <- run(m, nfold = 3, seed = 1, type = type)
        expect_length(res$folds, 3)
        expect_gt(res$mean, 0)
    }
})

test_that("two polytomous models can be compared", {
    skip_if_not_installed("mirt")
    d  <- sim_graded()
    m0 <- fit(d, itemtype = "Rasch")
    m1 <- fit(d, itemtype = "graded")
    expect_true(is.finite(run(m0, m1, nfold = 5, seed = 2, type = "t")$mean))
})

test_that("response codes with gaps give the same result as 0..K-1", {
    skip_if_not_installed("mirt")
    d  <- sim_graded()
    r1 <- run(fit(d, itemtype = "graded"), nfold = 3, seed = 3)
    r2 <- run(fit(as.data.frame(d * 2 + 1), itemtype = "graded"), nfold = 3, seed = 3)
    expect_equal(r1$folds, r2$folds)
})

test_that("type has no effect when all items are binary", {
    skip_if_not_installed("mirt")
    m  <- fit(as.data.frame((sim_graded() > 0) * 1))
    expect_equal(run(m, nfold = 3, seed = 4, type = "c"),
                 run(m, nfold = 3, seed = 4, type = "t"))
})

test_that("a category seen once is an informative error", {
    skip_if_not_installed("mirt")
    d <- sim_graded()
    d$item1[d$item1 == 2] <- 1
    d$item1[1] <- 2
    expect_error(suppressMessages(imv(fit(d, itemtype = "graded"), nfold = 3, seed = 5)),
                 "item1")
})

test_that("out-of-domain tasks give one summary naming them", {
    x  <- rep(0:2, each = 30)
    p  <- c(0.98, 0.01, 0.01)
    w  <- testthat::capture_warnings(imv_c(x, p, rep(1 / 3, 3), 3))
    expect_length(w, 1)
    expect_match(w, "baseline model: 2 of 3 category pairs")
    expect_match(w, "1 vs 2|0 vs 1|0 vs 2")
})

test_that("tagged warnings are counted per fold, not per task", {
    bad <- function(L) .imv_out_of_domain_warning("baseline", L)
    w <- testthat::capture_warnings(.tally_out_of_domain(5, for (f in 1:2) {
        for (task in c("0 vs 1", "0 vs 2")) {
            .tag_out_of_domain(bad(-1), task = task, item = "a", fold = f)
        }
    }))
    expect_length(w, 1)
    expect_match(w, "2 of 5 folds")
    expect_match(w, "a [0 vs 2]", fixed = TRUE)
})

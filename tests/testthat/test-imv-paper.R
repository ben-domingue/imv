## The default imv_c() and imv_t() reproduce the item-level omega_c and omega_t
## computed by the polytomous IMV paper's own code (Phase 4 pipeline) on two
## simulated tables. The fixture holds each item's pooled out-of-fold predictions
## and the paper code's values; it is built by data-raw/paper_sim.R. The paper
## used eps = 1e-6, so sigma is passed explicitly; all else is the package default.

items <- readRDS(test_path("fixtures", "paper_sim.rds"))

## the comparisons of the paper: baseline -> enhanced. "ctt" predicts each
## response from its item's category proportions in the training fold.
comparisons <- list(ctt_grm  = c("ctt", "grm"),  ctt_gpcm = c("ctt", "gpcm"),
                    ctt_nrm  = c("ctt", "nrm"),  gpcm_grm = c("gpcm", "grm"),
                    grm_nrm  = c("grm", "nrm"),  gpcm_nrm = c("gpcm", "nrm"))

## The cross-validated CTT baseline scores just below -log(2) on some
## near-balanced tasks, and imv_c/imv_t say so; the paper's code floors those
## tasks at a fair coin in the same way. Only these notices are muted.
quiet_ood <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
        if (grepl("below -log\\(2\\)", conditionMessage(w))) invokeRestart("muffleWarning")
    })
}

predictions <- function(it, model) {
    if (model == "ctt") it$ctt[it$fold, , drop = FALSE] else it$P[[model]]
}

test_that("fixture covers the two tables and six comparisons", {
    expect_length(items, 6)
    expect_setequal(unique(vapply(items, `[[`, "", "table")),
                    c("sim_k45", "sim_k7"))
    for (it in items) expect_named(it$c1, names(comparisons))
})

test_that("default imv_c() reproduces the paper's omega_c (c1)", {
    for (it in items) for (cn in names(comparisons)) {
        m <- comparisons[[cn]]
        expect_equal(
            as.numeric(quiet_ood(imv_c(it$x, predictions(it, m[1]), predictions(it, m[2]), K = it$K, sigma = 1e-6))),
            it$c1[[cn]], tolerance = 1e-10,
            label = paste(it$table, it$item, cn, "omega_c"))
    }
})

test_that("default imv_t() reproduces the paper's omega_t", {
    for (it in items) for (cn in names(comparisons)) {
        m <- comparisons[[cn]]
        expect_equal(
            as.numeric(quiet_ood(imv_t(it$x, predictions(it, m[1]), predictions(it, m[2]), K = it$K, sigma = 1e-6))),
            it$t[[cn]], tolerance = 1e-10,
            label = paste(it$table, it$item, cn, "omega_t"))
    }
})

## Builds tests/testthat/fixtures/paper_sim.rds, the inputs and reference values
## for test-imv-paper.R. Not part of the package build (see .Rbuildignore).
##
## The data are simulated, so the fixture redistributes no real responses. Two
## tables shaped like the small IRW tables of the polytomous IMV paper (Phase 4):
##   sim_k45  graded items with K = 4, 5, 5, 5, 5
##   sim_k7   graded items with K = 7; item5's top category has under 10 responses
## N = 300 persons, J = 5 items, 1% of responses missing at random. All five items
## are fitted; the fixture keeps three per table (item1-3 and item1, 2, 5).
##
## Both tables go through the paper's own code, unchanged (imv-poly repository):
## preprocess_table() from phase4/common.R stages them, and phase4/run_table.R
## fits CTT, GRM, GPCM and NRM with 4-fold response-level cross-fitting and
## computes the item IMVs. P4_KEEP_PRED=1 keeps the out-of-fold predictions.
## The fixture stores, per item, those predictions and the paper code's c1 and
## t (the "varadj" omega_t, i.e. the package's "continuity" weights), so the
## test never fits a model and does not depend on the mirt version.
##
## Usage (from the package root): Rscript data-raw/paper_sim.R <imv-poly dir>

paper <- normalizePath(commandArgs(trailingOnly = TRUE)[1])
work  <- tempfile("paper_sim_")
dir.create(file.path(work, "data"), recursive = TRUE)
dir.create(file.path(work, "results"))

P4_ROOT <- paper
source(file.path(paper, "phase4", "common.R"))

## graded-response data in long format, original codes 1..K
simulate <- function(K, N = 300, seed, sparse_top = integer(0)) {
    set.seed(seed)
    J <- length(K)
    a <- matrix(runif(J, 1, 2))
    d <- matrix(NA_real_, J, max(K) - 1)
    for (j in seq_len(J)) {
        d[j, seq_len(K[j] - 1)] <- sort(seq(2.2, -2.2, length.out = K[j] - 1) + rnorm(K[j] - 1, 0, 0.3),
                                       decreasing = TRUE)
        if (j %in% sparse_top) d[j, K[j] - 1] <- -4.5
    }
    X <- mirt::simdata(a, d, N, itemtype = "graded")
    X[sample(length(X), round(0.01 * length(X)))] <- NA
    obs <- which(!is.na(X), arr.ind = TRUE)
    data.frame(id = obs[, 1], item = paste0("item", obs[, 2]), resp = X[obs] + 1L)
}

K_sim  <- list(sim_k45 = c(4, 5, 5, 5, 5), sim_k7 = rep(7, 5))
tables <- list(sim_k45 = simulate(K_sim$sim_k45, seed = 101),
               sim_k7  = simulate(K_sim$sim_k7, seed = 102, sparse_top = 5))
## items stored in the fixture (all five are fitted), to keep it small
keep   <- list(sim_k45 = c("item1", "item2", "item3"), sim_k7 = c("item1", "item2", "item5"))
for (tb in names(tables)) {
    st <- preprocess_table(tables[[tb]], tb)
    stopifnot(all(st$items$polytomous), identical(as.numeric(st$items$K), K_sim[[tb]]))
    saveRDS(st, file.path(work, "data", paste0(tb, ".rds")))
    status <- system2(file.path(R.home("bin"), "Rscript"),
                      c(file.path(paper, "phase4", "run_table.R"), "--table", tb),
                      env = c(paste0("P4_DATA=", file.path(work, "data")),
                              paste0("P4_RESULTS=", file.path(work, "results")),
                              "P4_KEEP_PRED=1"))
    stopifnot(status == 0, file.exists(file.path(work, "results", tb, "done")))
}

models <- c("grm", "gpcm", "nrm")
cmps   <- c("ctt_grm", "ctt_gpcm", "ctt_nrm", "gpcm_grm", "grm_nrm", "gpcm_nrm")
items  <- list()
for (tb in names(tables)) {
    out <- file.path(work, "results", tb)
    st  <- readRDS(file.path(work, "data", paste0(tb, ".rds")))
    ref <- readRDS(file.path(out, "items.rds"))
    obs <- which(!is.na(st$X), arr.ind = TRUE)
    xl  <- st$X[obs]                          # long responses, as run_table.R orders them
    fr  <- lapply(models, function(m) lapply(1:4, function(f)
        readRDS(file.path(out, sprintf("fold%d_%s.rds", f, m)))))
    names(fr) <- models
    stopifnot(all(vapply(unlist(fr, recursive = FALSE), function(z) isTRUE(z$ok), TRUE)))
    for (j in which(st$items$item %in% keep[[tb]])) {
        pj   <- lapply(fr, function(fm) lapply(fm, function(z) z$pred[[as.character(j)]]))
        rows <- unlist(lapply(pj$grm, `[[`, "rows"))
        for (m in models) {
            stopifnot(identical(unlist(lapply(pj[[m]], `[[`, "rows")), rows),
                      identical(lapply(pj[[m]], `[[`, "ctt"), lapply(pj$grm, `[[`, "ctt")))
        }
        r <- ref[ref$item_index == j, ]
        stopifnot(nrow(r) == 1)
        it <- list(
            table = tb, item = st$items$item[j], K = st$items$K[j],
            x     = as.integer(xl[rows]),
            fold  = rep(1:4, vapply(pj$grm, function(p) length(p$rows), 1L)),
            ctt   = t(vapply(pj$grm, `[[`, numeric(st$items$K[j]), "ctt")),
            P     = lapply(pj, function(pm) do.call(rbind, lapply(pm, `[[`, "P"))),
            c1    = unlist(r[paste0(cmps, "_c1")]),
            t     = unlist(r[paste0(cmps, "_t_varadj")]))
        names(it$c1) <- names(it$t) <- cmps
        items[[length(items) + 1L]] <- it
    }
}
saveRDS(items, "tests/testthat/fixtures/paper_sim.rds", compress = "xz")
unlink(work, recursive = TRUE)

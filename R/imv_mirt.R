
makeresponse <- function(x,
                         remove.nonvarying.items = TRUE,
                         remove.allNA.rows = TRUE) {
    nms <- unique(x$item)
    if (all(nms %in% seq_along(nms))) x$item <- paste0("item_", x$item)
    id <- unique(x$id)
    L  <- split(x, x$item)
    out <- vector("list", length(L))
    for (i in seq_along(L)) {
        z        <- L[[i]]
        resp     <- rep(NA, length(id))
        resp[match(z$id, id)] <- z$resp
        out[[i]] <- resp
    }
    resp <- data.frame(do.call("cbind", out))
    names(resp) <- names(L)
    resp$id <- id
    if (remove.nonvarying.items) {
        nr   <- apply(resp, 2, function(x) length(table(x)))
        resp <- resp[, nr > 1, drop = FALSE]
    }
    if (remove.allNA.rows) resp <- resp[rowSums(!is.na(resp)) > 1, ]
    resp
}

.eval_mirt_call <- function(call_expr, train, caller_env) {
    env       <- new.env(parent = caller_env)
    env$train <- train
    eval(call_expr, envir = env)
}

.mirt_long_format <- function(mod) {
    x  <- mod@Data$data
    id <- seq_len(nrow(x))
    L  <- vector("list", ncol(x))
    for (i in seq_along(L)) {
        L[[i]] <- data.frame(id = id, item = colnames(x)[i], resp = x[, i])
    }
    x <- do.call("rbind", L)
    x[!is.na(x$resp), ]
}

.mirt_capture_call <- function(mod) {
    call <- deparse(mod@Call)
    call <- gsub("data\\s*=\\s*[^,)]+", "data = train", call)
    parse(text = call)
}

.mirt_get_preds <- function(mm, train_id, fscores.options, items) {
    th <- do.call(mirt::fscores, c(list(object = mm), fscores.options))
    ll <- vector("list", length(items))
    for (j in seq_along(items)) {
        it      <- mirt::extract.item(mm, items[j])
        pp      <- mirt::probtrace(it, th)
        ll[[j]] <- data.frame(id = train_id, item = items[j], pr = pp[, 2])
    }
    do.call("rbind", ll)
}

## Full probtrace() matrix for each item, rows in train_id order.
.mirt_get_prob_matrices <- function(mm, fscores.options, items) {
    th <- do.call(mirt::fscores, c(list(object = mm), fscores.options))
    ll <- lapply(items, function(item) mirt::probtrace(mirt::extract.item(mm, item), th))
    names(ll) <- items
    ll
}

## Does every training partition keep all persons and all items?
.mirt_folds_whole <- function(x, nfold, np, ni,
                              remove.nonvarying.items, remove.allNA.rows) {
    nps <- nis <- numeric(nfold)
    for (ii in seq_len(nfold)) {
        tr      <- makeresponse(x[x$group != ii, ],
                                remove.nonvarying.items = remove.nonvarying.items,
                                remove.allNA.rows = remove.allNA.rows)
        nps[ii] <- nrow(tr)
        nis[ii] <- ncol(tr) - 1L
    }
    all(nps == np) && all(nis == ni)
}

## Does every training partition contain every observed category of every item?
.mirt_folds_keep_categories <- function(x, nfold) {
    key   <- paste(x$item, x$resp, sep = "\r")
    total <- table(key)
    for (ii in seq_len(nfold)) {
        held <- table(key[x$group == ii])
        if (any(held >= total[names(held)])) return(FALSE)
    }
    TRUE
}

## Cross-validation for models with at least one polytomous item. Each fold
## builds every item's held-out cells and scores them with .mirt_poly_score().
.imv_mirt_poly <- function(m0, m1, x, c0, nfold, type, weights, aggregate, item.weights,
                           cats, fscores.options, whole.matrix,
                           remove.nonvarying.items, remove.allNA.rows, caller_env) {
    if (!is.null(m1) && !identical(m0@Data$data, m1@Data$data)) {
        stop("Models were fit on different data")
    }
    counts <- table(x$item, x$resp)
    rare   <- rownames(counts)[rowSums(counts == 1L) > 0L]
    if (length(rare) > 0L) {
        stop("A category with a single response cannot appear in every training ",
             "fold; collapse rare categories in item(s): ", paste(rare, collapse = ", "))
    }
    c1   <- if (!is.null(m1)) .mirt_capture_call(m1)
    np   <- length(unique(x$id))
    ni   <- length(unique(x$item))

    ## Unlike the binary path, categories are required even when
    ## whole.matrix = FALSE or m1 = NULL: probtrace() columns only line up with
    ## the full-data categories if the training fold saw all of them.
    check_whole <- whole.matrix && !is.null(m1)
    converged <- categories_found <- FALSE
    counter   <- 0L
    while (!converged && counter < 100L) {
        x$group <- sample(seq_len(nfold), nrow(x), replace = TRUE)
        has_categories   <- .mirt_folds_keep_categories(x, nfold)
        categories_found <- categories_found || has_categories
        converged <- has_categories &&
            (!check_whole || .mirt_folds_whole(x, nfold, np, ni, remove.nonvarying.items,
                                               remove.allNA.rows))
        counter <- counter + 1L
    }
    if (!converged && categories_found) {
        stop("Sample sizes don't support whole.matrix = TRUE; try whole.matrix = FALSE")
    }
    if (!converged) {
        stop("Could not find folds whose training data contain every category of ",
             "every item; collapse rare categories")
    }

    fold_imvs <- numeric(nfold)
    .tally_out_of_domain(nfold, for (i in seq_len(nfold)) {
        train_obs    <- makeresponse(x[x$group != i, ],
                                     remove.nonvarying.items = remove.nonvarying.items,
                                     remove.allNA.rows = remove.allNA.rows)
        train_id     <- train_obs$id
        train_obs$id <- NULL
        test  <- x[x$group == i, ]
        train <- x[x$group != i, ]
        items <- unique(test$item)
        pr0   <- if (!is.null(m1)) {
            .mirt_get_prob_matrices(.eval_mirt_call(c0, train_obs, caller_env),
                                    fscores.options, items)
        }
        mm1   <- .eval_mirt_call(if (is.null(m1)) c0 else c1, train_obs, caller_env)
        pr1   <- .mirt_get_prob_matrices(mm1, fscores.options, items)
        cells <- lapply(items, function(item) {
            .mirt_poly_cells(item, test, train, train_id, cats[[item]], pr0[[item]],
                             pr1[[item]])
        })
        names(cells) <- items
        fold_imvs[i] <- .mirt_poly_score(cells, type, weights, aggregate, item.weights, fold = i)
    }, unit = "folds")
    .build_imv_result(fold_imvs)
}

## An item's held-out cells: responses recoded 0..K-1 and the matching rows of the
## baseline and enhanced category probabilities, complete cases only (NULL if
## none). P0_train = NULL means the baseline is the item's training-fold
## category prevalence.
.mirt_poly_cells <- function(item, test, train, train_id, cats, P0_train, P1_train) {
    K <- length(cats)
    if (ncol(P1_train) != K || (!is.null(P0_train) && ncol(P0_train) != K)) {
        stop("Model for item '", item, "' has a different number of categories than ",
             "the data (", K, ")")
    }
    te  <- test[test$item == item, ]
    row <- match(te$id, train_id)
    P1  <- P1_train[row, , drop = FALSE]
    P0  <- if (is.null(P0_train)) {
        tr <- train$resp[train$item == item]
        matrix(tabulate(match(tr, cats), K) / length(tr), length(row), K, byrow = TRUE)
    } else {
        P0_train[row, , drop = FALSE]
    }
    keep <- stats::complete.cases(P0, P1)
    if (!any(keep)) return(NULL)
    list(x = match(te$resp[keep], cats) - 1L, P0 = P0[keep, , drop = FALSE],
         P1 = P1[keep, , drop = FALSE], K = K)
}

## One fold's IMV from its cells (a named list, one entry per item).
##   "items"   one omega per item, combined by .combine_items()
##   "pooled"  every item's cells stacked into one omega call (items share
##             one category set; checked before fitting)
.mirt_poly_score <- function(cells, type, weights, aggregate, item.weights, fold) {
    omega <- function(s) as.numeric(if (type == "c") .omega_c(s, 1e-4, weights)
                                    else .omega_t(s, 1e-4, weights))
    cells <- Filter(Negate(is.null), cells)
    if (length(cells) == 0L) return(NA_real_)
    if (aggregate == "pooled") {
        s <- .poly_setup(unlist(lapply(cells, `[[`, "x")),
                         do.call(rbind, lapply(cells, `[[`, "P0")),
                         do.call(rbind, lapply(cells, `[[`, "P1")), cells[[1]]$K)
        return(.tag_out_of_domain(omega(s), fold = fold))
    }
    imvs <- vapply(names(cells), function(item) {
        cl <- cells[[item]]
        .tag_out_of_domain(omega(.poly_setup(cl$x, cl$P0, cl$P1, cl$K)),
                           item = item, fold = fold)
    }, numeric(1))
    .combine_items(imvs, vapply(cells, function(cl) length(cl$x), numeric(1)), item.weights)
}

## One fold's IMV for dichotomous items: a single imv.binary() call on all held-out
## responses ("pooled", the original behaviour) or one per item ("items").
.mirt_binary_score <- function(resp, p0, p1, item, aggregate, item.weights, fold) {
    if (aggregate == "pooled") return(imv.binary(resp, p0, p1))
    by_item <- split(seq_along(resp), item)
    imvs <- vapply(names(by_item), function(it) {
        r <- by_item[[it]]
        .tag_out_of_domain(imv.binary(resp[r], p0[r], p1[r]), item = it, fold = fold)
    }, numeric(1))
    .combine_items(imvs, lengths(by_item), item.weights)
}

## Weighted mean of item IMVs: weights are the numbers of held-out responses
## scored ("responses") or 1 per item ("equal"). Items with an NA IMV or no
## scored responses drop out.
.combine_items <- function(imvs, n, item.weights) {
    ok <- !is.na(imvs) & n > 0
    if (!any(ok)) return(NA_real_)
    w <- if (item.weights == "responses") n[ok] else rep(1, sum(ok))
    sum(w * imvs[ok]) / sum(w)
}

## Stop before fitting when aggregate = "pooled" meets items whose category sets
## differ: pooled tasks across items are only meaningful for one response format.
.check_pooled_categories <- function(cats) {
    key <- vapply(cats, paste, "", collapse = ",")
    if (length(unique(key)) <= 1L) return(invisible())
    K <- lengths(cats)
    stop("aggregate = \"pooled\" needs every item to share one set of response ",
         "categories, but they differ: ",
         paste0(names(cats), " (K = ", K, ")", collapse = ", "),
         ". Use aggregate = \"items\" instead.", call. = FALSE)
}

imv.SingleGroupClass <- function(m0, m1 = NULL,
                                  data = NULL,
                                  nfold = 5,
                                  predict_fn = NULL,
                                  y = NULL,
                                  seed = NULL,
                                  fscores.options = list(method = "EAP"),
                                  whole.matrix = TRUE,
                                  remove.nonvarying.items = TRUE,
                                  remove.allNA.rows = TRUE,
                                  type = c("c", "t"),
                                  weights = c("continuity", "legacy"),
                                  aggregate = NULL,
                                  item.weights = c("responses", "equal"),
                                  ...) {
    if (!requireNamespace("mirt", quietly = TRUE)) {
        stop("Package 'mirt' is required. Install with: install.packages('mirt')")
    }
    type    <- match.arg(type)
    weights <- match.arg(weights)
    item.weights <- match.arg(item.weights)
    binary  <- all(m0@Data$K == 2)
    if (is.null(aggregate)) aggregate <- if (binary) "pooled" else "items"
    aggregate <- match.arg(aggregate, c("items", "pooled"))
    if (weights == "legacy") .warn_legacy_weights()

    caller_env <- parent.frame()
    x    <- .mirt_long_format(m0)
    cats <- lapply(split(x$resp, x$item), function(r) sort(unique(r)))
    if (aggregate == "pooled") .check_pooled_categories(cats)
    if (!is.null(seed)) set.seed(seed)
    c0 <- .mirt_capture_call(m0)

    if (!binary) {
        return(.imv_mirt_poly(m0, m1, x, c0, nfold, type, weights, aggregate, item.weights,
                              cats, fscores.options, whole.matrix,
                              remove.nonvarying.items, remove.allNA.rows, caller_env))
    }

    if (is.null(m1)) {
        x$group  <- sample(seq_len(nfold), nrow(x), replace = TRUE)
        fold_imvs <- numeric(nfold)
        .tally_out_of_domain(nfold, for (i in seq_len(nfold)) {
            train_obs    <- makeresponse(x[x$group != i, ],
                                         remove.nonvarying.items = remove.nonvarying.items,
                                         remove.allNA.rows = remove.allNA.rows)
            train_id     <- train_obs$id
            train_obs$id <- NULL
            mm    <- .eval_mirt_call(c0, train_obs, caller_env)
            items <- unique(x$item[x$group == i])
            preds <- .mirt_get_preds(mm, train_id, fscores.options, items)
            test  <- x[x$group == i, ]
            y     <- merge(test, preds, all.x = TRUE)
            ## baseline: each item's prevalence in the training fold
            train <- x[x$group != i, ]
            p_item <- tapply(train$resp, train$item, mean)
            y$p0  <- as.numeric(p_item[y$item])
            y     <- y[!is.na(y$pr) & !is.na(y$p0), ]
            fold_imvs[i] <- .mirt_binary_score(y$resp, y$p0, y$pr, y$item, aggregate,
                                               item.weights, fold = i)
        })
        return(.build_imv_result(fold_imvs))
    }

    if (!identical(m0@Data$data, m1@Data$data)) stop("Models were fit on different data")
    c1 <- .mirt_capture_call(m1)
    np <- length(unique(x$id))
    ni <- length(unique(x$item))

    if (whole.matrix) {
        converged <- FALSE
        counter   <- 0L
        while (!converged && counter < 100L) {
            x$group <- sample(seq_len(nfold), nrow(x), replace = TRUE)
            converged <- .mirt_folds_whole(x, nfold, np, ni, remove.nonvarying.items,
                                           remove.allNA.rows)
            counter   <- counter + 1L
        }
        if (!converged) stop("Sample sizes don't support whole.matrix = TRUE; try whole.matrix = FALSE")
    } else {
        x$group <- sample(seq_len(nfold), nrow(x), replace = TRUE)
    }

    fold_imvs <- numeric(nfold)
    .tally_out_of_domain(nfold, for (i in seq_len(nfold)) {
        train_obs    <- makeresponse(x[x$group != i, ],
                                     remove.nonvarying.items = remove.nonvarying.items,
                                     remove.allNA.rows = remove.allNA.rows)
        train_id     <- train_obs$id
        train_obs$id <- NULL
        mm0   <- .eval_mirt_call(c0, train_obs, caller_env)
        mm1   <- .eval_mirt_call(c1, train_obs, caller_env)
        items <- unique(x$item[x$group == i])
        pr0   <- .mirt_get_preds(mm0, train_id, fscores.options, items)
        pr1   <- .mirt_get_preds(mm1, train_id, fscores.options, items)
        names(pr0)[names(pr0) == "pr"] <- "pr0"
        names(pr1)[names(pr1) == "pr"] <- "pr1"
        preds <- merge(pr0, pr1, by = c("id", "item"))
        test  <- x[x$group == i, ]
        y     <- merge(test, preds, all.x = TRUE)
        y     <- y[!is.na(y$pr0) & !is.na(y$pr1), ]
        fold_imvs[i] <- .mirt_binary_score(y$resp, y$pr0, y$pr1, y$item, aggregate,
                                           item.weights, fold = i)
    })
    .build_imv_result(fold_imvs)
}

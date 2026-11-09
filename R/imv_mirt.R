
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
## scores every item on its held-out responses and averages across items.
.imv_mirt_poly <- function(m0, m1, x, c0, nfold, type, fscores.options, whole.matrix,
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
    cats <- lapply(split(x$resp, x$item), function(r) sort(unique(r)))
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
        item_imvs <- vapply(items, function(item) {
            .tag_out_of_domain(
                .mirt_poly_item(item, test, train, train_id, cats[[item]], pr0[[item]],
                                pr1[[item]], type),
                item = item, fold = i)
        }, numeric(1))
        fold_imvs[i] <- if (all(is.na(item_imvs))) NA_real_ else mean(item_imvs, na.rm = TRUE)
    }, unit = "folds")
    .build_imv_result(fold_imvs)
}

## Omega for one item on its held-out responses. P0_train = NULL means the
## baseline is the item's training-fold category prevalence.
.mirt_poly_item <- function(item, test, train, train_id, cats, P0_train, P1_train, type) {
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
    if (!any(keep)) return(NA_real_)
    s  <- .poly_setup(match(te$resp[keep], cats) - 1L, P0[keep, , drop = FALSE],
                      P1[keep, , drop = FALSE], K)
    as.numeric(if (type == "c") .omega_c(s, 1e-4) else .omega_t(s, 1e-4))
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
                                  ...) {
    if (!requireNamespace("mirt", quietly = TRUE)) {
        stop("Package 'mirt' is required. Install with: install.packages('mirt')")
    }
    type <- match.arg(type)
    if (!is.null(seed)) set.seed(seed)

    caller_env <- parent.frame()
    x  <- .mirt_long_format(m0)
    c0 <- .mirt_capture_call(m0)

    if (!all(m0@Data$K == 2)) {
        return(.imv_mirt_poly(m0, m1, x, c0, nfold, type, fscores.options, whole.matrix,
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
            y$p0  <- mean(x$resp[x$group != i], na.rm = TRUE)
            y     <- y[!is.na(y$pr), ]
            fold_imvs[i] <- imv.binary(y$resp, y$p0, y$pr)
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
        fold_imvs[i] <- imv.binary(y$resp, y$pr0, y$pr1)
    })
    .build_imv_result(fold_imvs)
}

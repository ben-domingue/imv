
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

imv.SingleGroupClass <- function(m0, m1 = NULL,
                                  data = NULL,
                                  nfold = 5,
                                  predict_fn = NULL,
                                  y = NULL,
                                  fscores.options = list(method = "EAP"),
                                  whole.matrix = TRUE,
                                  remove.nonvarying.items = TRUE,
                                  remove.allNA.rows = TRUE,
                                  ...) {
    if (!requireNamespace("mirt", quietly = TRUE)) {
        stop("Package 'mirt' is required. Install with: install.packages('mirt')")
    }
    if (!all(m0@Data$K == 2)) stop("imv() for mirt only supports dichotomous responses")

    caller_env <- parent.frame()
    x  <- .mirt_long_format(m0)
    c0 <- .mirt_capture_call(m0)

    if (is.null(m1)) {
        x$group  <- sample(seq_len(nfold), nrow(x), replace = TRUE)
        fold_imvs <- numeric(nfold)
        for (i in seq_len(nfold)) {
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
        }
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
            nps <- nis <- numeric(nfold)
            for (ii in seq_len(nfold)) {
                tr      <- makeresponse(x[x$group != ii, ],
                                        remove.nonvarying.items = remove.nonvarying.items,
                                        remove.allNA.rows = remove.allNA.rows)
                nps[ii] <- nrow(tr)
                nis[ii] <- ncol(tr) - 1L
            }
            converged <- all(nps == np) && all(nis == ni)
            counter   <- counter + 1L
        }
        if (!converged) stop("Sample sizes don't support whole.matrix = TRUE; try whole.matrix = FALSE")
    } else {
        x$group <- sample(seq_len(nfold), nrow(x), replace = TRUE)
    }

    fold_imvs <- numeric(nfold)
    for (i in seq_len(nfold)) {
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
    }
    .build_imv_result(fold_imvs)
}

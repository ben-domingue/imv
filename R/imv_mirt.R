
makeresponse <- function(x,
                         remove.nonvarying.items = TRUE,
                         remove.allNA.rows = TRUE
                         ) {
    ##make IR matrix
    nms <- unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item <- paste("item_", x$item, sep = '')
    ##make response matrix
    id <- unique(x$id)
    L  <- split(x, x$item)
    out <- list()
    for (i in 1:length(L)) {
        z     <- L[[i]]
        index <- match(z$id, id)
        resp  <- rep(NA, length(id))
        resp[index] <- z$resp
        out[[i]] <- resp
    }
    resp <- do.call("cbind", out)
    resp <- data.frame(resp)
    names(resp) <- names(L)
    resp$id <- id
    if (remove.nonvarying.items) {
        nr   <- apply(resp, 2, function(x) length(table(x)))
        resp <- resp[, nr > 1]
    }
    if (remove.allNA.rows) resp <- resp[rowSums(!is.na(resp)) > 1, ]
    resp
}

## Evaluate a captured mirt model call against a new 'train' dataset.
## The call text is evaluated in an environment that inherits from the mirt
## namespace so that mirt(), mirt.model(), etc. are visible without library().
.eval_mirt_call <- function(call_expr, train) {
    env        <- new.env(parent = loadNamespace("mirt"))
    env$train  <- train
    eval(call_expr, envir = env)
}

imv0mirt <- function(mod,
                     nfold = 5,
                     fscores.options = list(method = "EAP")
                     ) {
    if (!requireNamespace("mirt", quietly = TRUE)) {
        stop("Package 'mirt' is required. Install with: install.packages('mirt')")
    }

    x  <- mod@Data$data
    id <- 1:nrow(x)
    L  <- list()
    for (i in 1:ncol(x)) L[[i]] <- data.frame(id = id, item = colnames(x)[i], resp = x[, i])
    x <- data.frame(do.call("rbind", L))
    ##remove NA
    x <- x[!is.na(x$resp), ]
    ##
    x$group <- sample(1:nfold, nrow(x), replace = TRUE)
    ##
    call <- mod@Call
    call <- deparse(call)
    call <- gsub("data\\s*=\\s*[^,]+", "data = train", call) ##thanks lijin!
    call <- parse(text = call)
    ##
    om <- numeric()
    for (i in 1:nfold) {
        train <- makeresponse(x[x$group != i, ])
        id    <- train$id
        train$id <- NULL
        mm <- .eval_mirt_call(call, train)
        th <- do.call(mirt::fscores, c(list(object = mm), fscores.options))
        test  <- x[x$group == i, ]
        ll    <- list()
        items <- unique(test$item)
        for (j in 1:length(items)) {
            it <- items[j]
            it <- mirt::extract.item(mm, it)
            pp <- mirt::probtrace(it, th[, 1])
            ll[[j]] <- data.frame(id = id, item = names(mirt::coef(mm))[j], pr = pp[, 2])
        }
        y <- data.frame(do.call("rbind", ll))
        y <- merge(test, y, all.x = TRUE)
        y$p0 <- mean(x$resp[x$group != i], na.rm = TRUE)
        y  <- y[!is.na(y$pr), ]
        om[i] <- imv.binary(y$resp, y$p0, y$pr)
    }
    om
}

imv.mirt <- function(mod1,
                     mod2 = NULL,
                     nfold = 5,
                     fscores.options = list(method = "EAP"),
                     whole.matrix = TRUE,
                     ...) {
    if (!requireNamespace("mirt", quietly = TRUE)) {
        stop("Package 'mirt' is required. Install with: install.packages('mirt')")
    }

    kk <- mod1@Data$K
    if (!all(kk == 2)) stop("only works for dichotomous responses")
    x <- mod1@Data$data
    if (is.null(mod2)) {
        return(imv0mirt(mod1, nfold = nfold, fscores.options = fscores.options, ...))
    }
    x2 <- mod2@Data$data
    if (!identical(x, x2)) stop("Models run on different data")
    id <- 1:nrow(x)
    L  <- list()
    for (i in 1:ncol(x)) L[[i]] <- data.frame(id = id, item = colnames(x)[i], resp = x[, i])
    x <- data.frame(do.call("rbind", L))
    ##remove NA
    x <- x[!is.na(x$resp), ]
    ##
    np <- length(unique(x$id))
    ni <- length(unique(x$item))
    if (whole.matrix) {
        counter <- 1
        test    <- FALSE
        while (!test & counter < 100) {
            x$group <- sample(1:nfold, nrow(x), replace = TRUE)
            nps <- nis <- numeric()
            for (ii in 1:nfold) {
                train    <- makeresponse(x[x$group != ii, ], remove.nonvarying.items = TRUE)
                nps[ii]  <- nrow(train)
                nis[ii]  <- ncol(train) - 1 #no items
            }
            test1 <- all(nps == np)
            test2 <- all(nis == ni)
            test  <- test2 & test1
            counter <- counter + 1
        }
    } else x$group <- sample(1:nfold, nrow(x), replace = TRUE)
    if (!test & counter >= 100)
        stop("sample sizes don't support whole.matrix=TRUE")
    ##
    getcall <- function(mod) {
        call <- mod@Call
        call <- deparse(call)
        call <- gsub("data\\s*=\\s*[^,]+", "data = train", call) ##thanks lijin!
        call <- parse(text = call)
        call
    }
    c1 <- getcall(mod1)
    c2 <- getcall(mod2)
    ##
    om <- numeric()
    for (i in 1:nfold) {
        ##get training data, estimate models
        train <- makeresponse(x[x$group != i, ], ...)
        id    <- train$id
        train$id <- NULL
        mm1 <- .eval_mirt_call(c1, train)
        mm2 <- .eval_mirt_call(c2, train)
        ##get ability estimates
        th1 <- do.call(mirt::fscores, c(list(object = mm1), fscores.options))
        th2 <- do.call(mirt::fscores, c(list(object = mm2), fscores.options))
        ##get fitted values
        ll    <- list()
        items <- unique(x$item)
        for (j in 1:length(items)) {
            item <- items[j]
            it   <- mirt::extract.item(mm1, item)
            pp1  <- mirt::probtrace(it, th1[, 1])
            it   <- mirt::extract.item(mm2, item)
            pp2  <- mirt::probtrace(it, th2[, 1])
            ll[[j]] <- data.frame(id = id, item = item, pr1 = pp1[, 2], pr2 = pp2[, 2])
        }
        y    <- data.frame(do.call("rbind", ll))
        test <- x[x$group == i, ]
        y    <- merge(test, y, all.x = TRUE)
        ##compute imv
        om[i] <- imv.binary(y$resp, y$pr1, y$pr2)
    }
    return(om)
}

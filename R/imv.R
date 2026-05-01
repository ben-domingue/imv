imv <- function(m0, m1, ...) {
    UseMethod("imv")
}

imv.default <- function(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL, ...) {
    if (is.null(predict_fn)) {
        stop(
            "No imv method for class '", paste(class(m0), collapse = "/"), "'. ",
            "Supply predict_fn = function(model, newdata) returning a probability vector."
        )
    }
    if (is.null(data)) stop("'data' must be provided when using predict_fn")
    if (is.null(y))    stop("'y' (outcome column name) must be provided when using predict_fn")
    if (!y %in% names(data)) stop("column '", y, "' not found in data")

    n         <- nrow(data)
    fold_idx  <- sample(rep_len(seq_len(nfold), n))
    fold_imvs <- numeric(nfold)
    for (i in seq_len(nfold)) {
        test <- data[fold_idx == i, , drop = FALSE]
        fold_imvs[i] <- imv.binary(
            test[[y]],
            predict_fn(m0, test),
            predict_fn(m1, test)
        )
    }
    .build_imv_result(fold_imvs)
}

## Internal helpers used by all methods -----------------------------------

.cv_fold_imv <- function(data, outcome_var, nfold, refit_fn) {
    n         <- nrow(data)
    fold_idx  <- sample(rep_len(seq_len(nfold), n))
    fold_imvs <- numeric(nfold)
    for (i in seq_len(nfold)) {
        train <- data[fold_idx != i, , drop = FALSE]
        test  <- data[fold_idx == i, , drop = FALSE]
        preds <- refit_fn(train, test)
        fold_imvs[i] <- imv.binary(test[[outcome_var]], preds$p0, preds$p1)
    }
    fold_imvs
}

.build_imv_result <- function(fold_imvs) {
    m  <- mean(fold_imvs)
    s  <- sd(fold_imvs)
    se <- s / sqrt(length(fold_imvs))
    list(
        folds = fold_imvs,
        mean  = m,
        sd    = s,
        ci    = c(lower = m - 1.96 * se, upper = m + 1.96 * se)
    )
}

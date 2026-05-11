imv.workflow <- function(m0, m1, data = NULL, nfold = 5, predict_fn = NULL, y = NULL, ...) {
    if (!requireNamespace("workflows", quietly = TRUE)) {
        stop("Package 'workflows' is required for workflow models. ",
             "Install with: install.packages('workflows')")
    }
    if (is.null(data)) {
        stop("'data' must be provided for workflow models.")
    }
    if (is.null(y)) {
        y <- tryCatch({
            prep <- workflows::extract_preprocessor(m1)
            all.vars(prep)[1]
        }, error = function(e) NULL)
        if (is.null(y)) {
            stop(
                "'y' (outcome column name) must be provided for recipe-based workflows."
            )
        }
    }
    if (!y %in% names(data)) {
        stop("column '", y, "' not found in data")
    }

    data <- data[complete.cases(data), , drop = FALSE]
    if (nrow(data) < nfold) {
        stop(
            "Only ", nrow(data), " complete rows but nfold = ", nfold, ". ",
            "Reduce nfold or supply more data."
        )
    }

    # Keep factor outcome in data for workflow fitting; extract numeric for imv.binary
    y_num <- if (is.factor(data[[y]])) as.integer(data[[y]]) - 1L else data[[y]]

    n        <- nrow(data)
    fold_idx <- sample(rep_len(seq_len(nfold), n))
    fold_imvs <- numeric(nfold)
    for (i in seq_len(nfold)) {
        train   <- data[fold_idx != i, , drop = FALSE]
        test    <- data[fold_idx == i, , drop = FALSE]
        test_y  <- y_num[fold_idx == i]
        mm0     <- workflows::fit(m0, data = train)
        mm1     <- workflows::fit(m1, data = train)
        p0_raw  <- predict(mm0, new_data = test, type = "prob")
        p1_raw  <- predict(mm1, new_data = test, type = "prob")
        pos_col <- grep("^\\.pred_", names(p0_raw), value = TRUE)[2]
        fold_imvs[i] <- imv.binary(test_y, p0_raw[[pos_col]], p1_raw[[pos_col]])
    }
    .build_imv_result(fold_imvs)
}

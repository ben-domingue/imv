imv.ranger <- function(m0, m1, data = NULL, nfold = 5, predict_fn = NULL, y = NULL, ...) {
    if (!requireNamespace("ranger", quietly = TRUE)) {
        stop("Package 'ranger' is required for ranger models. ",
             "Install with: install.packages('ranger')")
    }
    for (m in list(m0, m1)) {
        if (!identical(m$treetype, "Probability estimation")) {
            stop(
                "imv() for ranger requires models fit with probability = TRUE. ",
                "Got treetype: '", m$treetype, "'."
            )
        }
    }
    if (is.null(data)) {
        stop(
            "ranger models do not store training data internally. ",
            "Refit with an explicit 'data' argument or pass 'data' to imv()."
        )
    }

    outcome_var  <- m1$dependent.variable.name
    vars0        <- c(m0$dependent.variable.name, m0$forest$independent.variable.names)
    vars1        <- c(outcome_var, m1$forest$independent.variable.names)
    all_vars     <- intersect(union(vars0, vars1), names(data))
    data         <- data[complete.cases(data[, all_vars, drop = FALSE]), , drop = FALSE]

    if (nrow(data) < nfold) {
        stop(
            "Only ", nrow(data), " complete rows but nfold = ", nfold, ". ",
            "Reduce nfold or supply more data."
        )
    }

    # Keep factor outcome in data for ranger fitting; extract numeric for imv.binary
    y_num <- if (is.factor(data[[outcome_var]])) as.integer(data[[outcome_var]]) - 1L
             else data[[outcome_var]]

    n         <- nrow(data)
    fold_idx  <- sample(rep_len(seq_len(nfold), n))
    fold_imvs <- numeric(nfold)
    for (i in seq_len(nfold)) {
        train  <- data[fold_idx != i, , drop = FALSE]
        test   <- data[fold_idx == i, , drop = FALSE]
        test_y <- y_num[fold_idx == i]
        mm0 <- ranger::ranger(
            dependent.variable.name = m0$dependent.variable.name,
            data          = train[, intersect(vars0, names(train)), drop = FALSE],
            num.trees     = m0$num.trees,
            mtry          = m0$mtry,
            min.node.size = m0$min.node.size,
            probability   = TRUE,
            verbose       = FALSE
        )
        mm1 <- ranger::ranger(
            dependent.variable.name = outcome_var,
            data          = train[, intersect(vars1, names(train)), drop = FALSE],
            num.trees     = m1$num.trees,
            mtry          = m1$mtry,
            min.node.size = m1$min.node.size,
            probability   = TRUE,
            verbose       = FALSE
        )
        fold_imvs[i] <- imv.binary(
            test_y,
            predict(mm0, data = test)$predictions[, 2],
            predict(mm1, data = test)$predictions[, 2]
        )
    }
    .build_imv_result(fold_imvs)
}

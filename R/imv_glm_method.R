imv.glm <- function(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL) {
    if (is.null(data)) {
        data <- m1$data
        if (is.null(data)) {
            stop(
                "Could not extract data from the model. ",
                "Refit with an explicit 'data' argument or pass 'data' to imv()."
            )
        }
    }

    outcome_var  <- all.vars(formula(m1))[1]
    vars_needed  <- union(all.vars(formula(m0)), all.vars(formula(m1)))
    vars_present <- vars_needed[vars_needed %in% names(data)]
    data <- data[complete.cases(data[, vars_present, drop = FALSE]), , drop = FALSE]

    if (nrow(data) < nfold) {
        stop(
            "Only ", nrow(data), " complete rows but nfold = ", nfold, ". ",
            "Reduce nfold or supply more data."
        )
    }

    fold_imvs <- .cv_fold_imv(data, outcome_var, nfold, function(train, test) {
        mm0 <- update(m0, data = train)
        mm1 <- update(m1, data = train)
        list(
            p0 = predict(mm0, newdata = test, type = "response"),
            p1 = predict(mm1, newdata = test, type = "response")
        )
    })

    .build_imv_result(fold_imvs)
}

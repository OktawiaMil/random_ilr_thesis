# File with the helper functions used in the "03_" scripts

# Function that trains selected model on the data expressed as proportion
train_model <- function(
    train_data,
    test_data,
    model = c(
        "random_forest",
        "xgboost",
        "l1_logistic_reg"
    ),
    trees = 100,
    model_seed = 2025,
    cv_nfolds = 10
) {
    model <- match.arg(model)

    # Ensure outcome is a binary factor
    if (!is.factor(train_data$outcome)) {
        train_data <- train_data |> mutate(outcome = as.factor(outcome))
    }
    if (!is.factor(test_data$outcome)) {
        test_data <- test_data |> mutate(outcome = as.factor(outcome))
    }
    lv <- levels(train_data$outcome)
    if (length(lv) != 2) {
        stop(
            "`outcome` must be a binary factor with exactly 2 levels in train_data."
        )
    }
    # Positive = second level; relevel to keep ordering consistent
    positive_level <- lv[2]
    negative_level <- lv[1]
    ordered_levels <- c(negative_level, positive_level)
    train_data <- train_data |>
        mutate(outcome = factor(outcome, levels = ordered_levels))
    test_data <- test_data |>
        mutate(outcome = factor(outcome, levels = ordered_levels))

    # RF or XGBoost path
    if (model %in% c("random_forest", "xgboost")) {
        if (model == "random_forest") {
            model_spec <- rand_forest(trees = trees) |>
                set_mode("classification") |>
                set_engine(
                    "ranger",
                    seed = model_seed,
                    num.threads = 1,
                    probability = TRUE
                )
        } else {
            set.seed(model_seed)
            model_spec <- boost_tree(trees = trees) |>
                set_mode("classification") |>
                set_engine("xgboost", nthread = 1)
        }

        rec <- recipe(outcome ~ ., data = train_data) |>
            step_zv(all_predictors())

        workflow_spec <- workflow() |>
            add_recipe(rec) |>
            add_model(model_spec)

        # Fit model
        model_fit <- fit(workflow_spec, data = train_data)

        # Predict on test data; extract prob of positive level
        prob <- predict(model_fit, test_data, type = "prob") |>
            rename(
                .pred_negative = all_of(paste0(".pred_", negative_level)),
                .pred_positive = all_of(paste0(".pred_", positive_level))
            )
        class <- predict(model_fit, test_data)
        preds <- bind_cols(test_data |> select(outcome), prob, class)

        metrics <- bind_rows(
            brier_class(
                preds,
                truth = outcome,
                .pred_negative
            ),
            roc_auc(
                preds,
                truth = outcome,
                .pred_negative
            ),
            accuracy(preds, truth = outcome, estimate = .pred_class)
        )

        roc_tbl <- roc_curve(
            preds,
            truth = outcome,
            .pred_negative
        )

        # Cleanup
        rm(model_fit)
        gc(FALSE)

        return(list(perf_metrics = metrics, roc_curve = roc_tbl))
    } else if (model == "l1_logistic_reg") {
        # L1-penalized logistic regression
        set.seed(model_seed)
        x_train <- train_data |> select(-c(outcome)) |> as.matrix()
        y_train <- train_data |> pull(outcome)
        x_test <- test_data |> select(-c(outcome)) |> as.matrix()
        y_test <- test_data |> pull(outcome)

        fit <- cv.glmnet(
            x = x_train,
            y = y_train,
            family = "binomial",
            nfolds = cv_nfolds
        )

        # Save estimated beta coefs. corresponding to lambda 1-se
        beta_1se <- coef(fit, s = "lambda.1se")
        beta_df <- data.frame(
            term = rownames(beta_1se),
            estimate = as.numeric(beta_1se),
            row.names = NULL
        )

        sel_lambda <- fit$lambda.1se
        pred_prob <- predict(
            fit,
            newx = x_test,
            s = sel_lambda,
            type = "response"
        ) |>
            as.numeric()
        pred_class <- if_else(
            pred_prob > 0.5,
            positive_level,
            negative_level
        ) |>
            factor(levels = c(negative_level, positive_level))

        pred_tbl <- tibble(
            truth = y_test,
            .pred_class = pred_class,
            .prob_positive = pred_prob,
            .prob_negative = 1 - .prob_positive
        )

        roc_tbl <- roc_curve(
            pred_tbl,
            truth = truth,
            .prob_positive,
            event_level = "second"
        )

        metrics <- bind_rows(
            brier_class(
                pred_tbl,
                truth = truth,
                .prob_negative
            ),
            roc_auc(
                pred_tbl,
                truth = truth,
                .prob_positive,
                event_level = "second"
            ),
            accuracy(pred_tbl, truth = truth, estimate = .pred_class)
        )

        rm(fit)
        gc(FALSE)

        return(list(
            perf_metrics = metrics,
            roc_curve = roc_tbl,
            lambda_1se = sel_lambda,
            beta_1se = beta_df
        ))
    }
}


# Helper: fit  all 3 models (RF, XGB, logistic regression with L1 penalty) on a given dataset
# Save results for each model as separate file in output_dir
fit_and_save_one_split <- function(
    train_df,
    test_df,
    split_seed,
    output_dir,
    train_idx = NULL,
    test_idx = NULL,
    aug_strategy = NULL,
    aug_factor = NULL
) {
    models <- c("random_forest", "xgboost", "l1_logistic_reg")
    model_short <- c(
        random_forest = "RF",
        xgboost = "XGB",
        l1_logistic_reg = "lasso"
    )

    for (m in models) {
        tree_arg <- if (m == "random_forest") {
            rf_trees
        } else if (m == "xgboost") {
            xgb_trees
        } else {
            NA_integer_
        }

        res <- train_model(
            train_data = train_df,
            test_data = test_df,
            model = m,
            trees = tree_arg,
            model_seed = model_seed
        )

        lambda_1se <- if (!is.null(res$lambda_1se)) {
            as.numeric(res$lambda_1se)
        } else {
            NA_real_
        }

        beta_1se <- if (!is.null(res$beta_1se)) res$beta_1se else NA

        out <- list(
            split_seed = split_seed,
            aug_strategy = aug_strategy,
            aug_factor = aug_factor,
            model = m,
            trees = tree_arg,
            train_idx = train_idx,
            test_idx = test_idx,
            perf_metrics = res$perf_metrics,
            roc_curve = res$roc_curve,
            lambda_1se = lambda_1se,
            beta_1se = beta_1se
        )

        saveRDS(
            out,
            file.path(
                output_dir,
                paste0(
                    model_short[[m]],
                    "_split_",
                    split_seed,
                    if (!is.null(aug_strategy)) {
                        paste0("_", aug_strategy)
                    } else {
                        "_benchmark"
                    },
                    if (!is.null(aug_factor)) {
                        paste0("_augf_", aug_factor)
                    } else {
                        ""
                    },
                    ".rds"
                )
            )
        )
    }
}

# Function that fits and saves the results of sparse log contrast
# classification model trained on the data expressed as log(x + PC),
# x - observed OTUs (absolute abundance)
sparse_log_cont_custom <- function(
    train_data,
    test_data,
    model_seed = 2025,
    cv_nfolds = 5,
    min_frac = 1e-04, #default setting in trac v. 0.0.2
    split_seed,
    output_dir,
    train_idx = NULL,
    test_idx = NULL,
    aug_strategy = NULL,
    aug_factor = NULL
) {
    set.seed(model_seed)
    # Prepare data
    z_tr <- train_data |> select(-outcome) |> as.matrix()
    y_tr <- train_data |>
        mutate(outcome = if_else(outcome == "1", 1, -1)) |>
        pull(outcome)

    z_test <- test_data |> select(-outcome) |> as.matrix()
    y_test <- test_data |>
        mutate(outcome = if_else(outcome == "1", 1, -1)) |>
        pull(outcome)

    # In the below code:
    # min_frac - the smalles k, k \in [0, 1) that describes the relation
    # lambda/lambda_max where lambda_max is the biggest value of lambda
    # parameter for which \beta != 0 (in a vector sense, so at least 1 entry != 0)
    # For a fixed value of nlam, if we set smaller value of min_frac then
    # the frac_list covers wider range of fractions between lambda and lambda_max
    # tested (for smaller min_frac the range of frac_list is (1, val_min) where
    # val_min is smaller than it would be for bigger value of min_frac) --> we are
    # allowing for the lambdas close to teh lambda_max --> sparser models are tested

    fit_log_contrast <- sparse_log_contrast(
        Z = z_tr,
        y = y_tr,
        min_frac = min_frac,
        nlam = 20, #default setting in trac v. 0.0.2
        method = "classif"
    )

    # Lambda selection: 5-folds CV which identifies the largest lambda whose
    # CV error is within one standard error of the minimum CV error (1-se lambda)
    # Below you pass fit_log_contrast because it allows to determine the range of
    # lambda values that should be tested
    cvfit_log_contrast <- cv_sparse_log_contrast(
        fit_log_contrast,
        Z = z_tr,
        y = y_tr,
        nfolds = cv_nfolds
    )

    # Index of the lambda parameter in the fraclist that leads
    # to the predictive perfromance corresponding to lambda = 1se:
    lambda_1se_idx <- cvfit_log_contrast$cv$i1se

    # Value of lambda 1-se
    lambda_1se <- cvfit_log_contrast$cv$lambda_1se
    # beta coefs corresponding to lambda 1-se
    beta_1se <- data.frame(
        estimate = fit_log_contrast$beta[, lambda_1se_idx]
    ) |>
        tibble::rownames_to_column(var = "term") |>
        tibble::add_row(
            term = "(Intercept)",
            estimate = fit_log_contrast$beta0[lambda_1se_idx]
        )

    # Predictions for all of fitted models (models fitted on all considered lambdas)
    # Values in pred are numeric values, can be negative or larger than 1
    # (meanwhile we have 1 and -1) - those are scores that are transformed
    # into binary labels (output can be controlled via output = c("raw",
    # "probability", "class"))
    # Output - raw scores
    pred_score <- predict_sparse_log_contrast(
        fit_log_contrast,
        new_Z = z_test
    )

    # Get into the predictions of the model with lambda-1se
    pred_score_1se <- pred_score[, lambda_1se_idx]
    # Turn predicted scores into class labels
    pred_class_1se <- if_else(pred_score_1se >= 0, 1, -1)

    pred_tbl <- tibble(
        truth = factor(y_test, levels = c(-1, 1)),
        .pred_class = factor(pred_class_1se, levels = c(-1, 1)),
        .pred_score = pred_score_1se
    )

    # Calculate ROC curve and performance metrics
    roc_tbl <- roc_curve(
        pred_tbl,
        truth = truth,
        .pred_score,
        event_level = "second"
    )

    metrics <- bind_rows(
        roc_auc(pred_tbl, truth = truth, .pred_score, event_level = "second"),
        accuracy(pred_tbl, truth = truth, estimate = .pred_class)
    )

    res <- list(perf_metrics = metrics, roc_curve = roc_tbl)

    out <- list(
        split_seed = split_seed,
        aug_strategy = aug_strategy,
        aug_factor = aug_factor,
        model = "sparse_log_contrast",
        trees = NULL,
        lambda_1se = lambda_1se,
        beta_1se = beta_1se,
        train_idx = train_idx,
        test_idx = test_idx,
        perf_metrics = res$perf_metrics,
        roc_curve = res$roc_curve
    )

    saveRDS(
        out,
        file.path(
            output_dir,
            paste0(
                "slc_split_",
                split_seed,
                if (!is.null(aug_strategy)) {
                    paste0("_", aug_strategy)
                } else {
                    "_benchmark"
                },
                if (!is.null(aug_factor)) {
                    paste0("_augf_", aug_factor)
                } else {
                    ""
                },
                ".rds"
            )
        )
    )
}

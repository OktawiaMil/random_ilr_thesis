# File with the helper functions used in the "03_" scripts

# Function that trains selected model on the
train_model <- function(
    train_data,
    test_data,
    model = c(
        "random_forest",
        "xgboost",
        "sparse_log_contrast",
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
    } else if (model == "sparse_log_contrast") {
        #TODO: implement custom function for the sparse log contrast that
        # will be suitable for classification - trac is good only for regression
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

        return(list(perf_metrics = metrics, roc_curve = roc_tbl))
    }
}


# Helper: fit  all 3 models (RF, XGB, sparse log-contrast) on a given dataset
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
    #models <- c("random_forest", "xgboost", "sparse_log_contrast")
    models <- c("random_forest", "xgboost", "l1_logistic_reg")
    model_short <- c(
        random_forest = "RF",
        xgboost = "XGB",
        sparse_log_contrast = "SLC",
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

        out <- list(
            split_seed = split_seed,
            aug_strategy = aug_strategy,
            aug_factor = aug_factor,
            model = m,
            trees = tree_arg,
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

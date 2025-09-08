## Libraries used by this helper
library(tidymodels)
library(glmnet)
library(dplyr)
library(tibble)

#' Train and evaluate a classification model
#'
#' Fits one of: LASSO (glmnet), Random Forest (ranger via parsnip), or
#' XGBoost (xgboost via parsnip) on `train_data`, evaluates on `test_data`,
#' and returns performance metrics along with the ROC curve. The positive class
#' is consistently the second level of the `outcome` factor, and ROC/AUC are
#' computed with `event_level = "second"`.
#'
#' @param train_data Data frame; predictors plus factor column `outcome`.
#' @param test_data Data frame; predictors plus factor column `outcome`.
#' @param model Character; one of "lasso", "random_forest", "xgboost".
#' @param loss Character; LASSO loss function, one of "deviance" or "auc".
#' @param trees Integer; number of trees (RF/XGBoost).
#' @param model_seed Integer; RNG seed for model fitting.
#' @param my_metrics A yardstick `metric_set`, e.g.
#'  `metric_set(accuracy, roc_auc, brier_class)`.
#' @param cv_nfolds Integer; number of CV folds for `cv.glmnet`.
#' @return A list with:
#'   - `perf_metrics`: tibble with metrics as specified by `my_metrics`,
#'   - `roc_curve`: tibble with ROC curve.
#'
#' @details
#' The function enforces a consistent positive-class convention: the
#' positive class is the second factor level of `outcome`. For LASSO,
#' the target is encoded as 0/1 with 1 = positive. For RF/XGBoost, the
#' predicted probability of the positive class is extracted from
#' `.pred_<positive>` and used for ROC/AUC and Brier.
#'
#' @export
train_evaluate_one <- function(
  train_data,
  test_data,
  model = c("lasso", "random_forest", "xgboost", "lasso_ilr"),
  loss = c("deviance", "auc"),
  trees = 100,
  model_seed = 2025,
  my_metrics = metric_set(accuracy, roc_auc, brier_class),
  cv_nfolds = 10
) {
  model <- match.arg(model)
  loss <- match.arg(loss)

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

  # Positive = second level
  positive_level <- lv[2]
  negative_level <- lv[1]
  new_levels <- c(negative_level, positive_level)
  train_data <- train_data |>
    mutate(outcome = factor(outcome, levels = new_levels))
  test_data <- test_data |>
    mutate(outcome = factor(outcome, levels = new_levels))

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

    # Predict on test data; extract prob of positive (second) level
    prob <- predict(model_fit, test_data, type = "prob") |>
      rename(.pred_positive = all_of(paste0(".pred_", positive_level)))
    class <- predict(model_fit, test_data)
    preds <- bind_cols(test_data |> select(outcome), prob, class)

    # ROC curve and metrics with explicit event_level
    roc_tbl <- roc_curve(
      preds,
      truth = outcome,
      .pred_positive,
      event_level = "second"
    )
    metrics <- my_metrics(
      preds,
      truth = outcome,
      estimate = .pred_class,
      .pred_positive,
      event_level = "second"
    )

    # Cleanup
    rm(model_fit)
    gc(FALSE)

    return(list(perf_metrics = metrics, roc_curve = roc_tbl))
  } else {
    # LASSO
    set.seed(model_seed)
    x_train <- train_data |> select(-c(outcome)) |> as.matrix()
    y_train <- train_data |> pull(outcome)
    x_test <- test_data |> select(-c(outcome)) |> as.matrix()
    y_test <- test_data |> pull(outcome)

    fit <- cv.glmnet(
      x = x_train,
      y = y_train,
      family = "binomial",
      type.measure = loss,
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
    pred_class <- if_else(pred_prob > 0.5, positive_level, negative_level) |>
      factor(levels = c(negative_level, positive_level))

    pred_tbl <- tibble(
      truth = y_test,
      .pred_class = pred_class,
      .prob_positive = pred_prob
    )

    roc_tbl <- roc_curve(
      pred_tbl,
      truth = truth,
      .prob_positive,
      event_level = "second"
    )
    metrics <- my_metrics(
      data = pred_tbl,
      truth = truth,
      estimate = .pred_class,
      .prob_positive,
      event_level = "second"
    )

    rm(fit)
    gc(FALSE)

    return(list(perf_metrics = metrics, roc_curve = roc_tbl))
  }
}

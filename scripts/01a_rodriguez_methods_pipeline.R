## Pipeline for Rodriguez augmentation methods:
## Aitchison Mixup, Compositional feature dropout, Compositional cutmix
##
## Workflow:
## - Create a stratified by outcome train/test split
## - For each augmentation factor f in max_factor:2, augment the training set
##   with aug_data_rodriguez(train_x, train_y, ...)
## - Train 4 models (lasso, random_forest, xgboost, lasso_ilr) and evaluate on
##   the fixed test set; save results to out_dir

## Libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidymodels)
  library(glmnet)
  library(augmenter)
  library(compositions)
})

## Robust sourcing of helpers from R/ folder
script_args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub(
  "--file=",
  "",
  script_args[grep("^--file=", script_args)]
))
script_dir <- dirname(script_path)
repo_root <- dirname(script_dir)

helpers_file <- file.path(repo_root, "R", "01a_rodriguez_helpers.R")
train_eval_file <- file.path(repo_root, "R", "01_helper_functions.R")
if (file.exists(helpers_file)) {
  source(helpers_file)
}
if (file.exists(train_eval_file)) {
  source(train_eval_file)
}

## Argument parsing ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 7) {
  stop(
    paste0(
      "Expected 7 args: data_dir data_id aug_strategy split_seed max_factor ",
      "pseudo_count out_dir\n",
      "Got ",
      length(args),
      ": ",
      paste(args, collapse = " ")
    )
  )
}

data_dir <- args[1]
data_id <- args[2]
# one of: "aitchison_mixup", "comp_feature", "comp_cutmix":
aug_strategy <- args[3]
split_seed <- as.integer(args[4])
max_factor <- as.integer(args[5])
pseudo_count <- args[6]
out_dir <- args[7]

if (is.na(max_factor) || max_factor < 2) {
  stop("max_factor must be an integer >= 2")
}

##  Data ----
# Choose input file by pseudo_count flag
if (pseudo_count == "half") {
  file_x <- file.path(
    data_dir,
    paste0("task", data_id, "_preproc_x_pc_half.csv")
  )
} else if (pseudo_count == "max_lib_size") {
  file_x <- file.path(
    data_dir,
    paste0("task", data_id, "_preproc_x_pc_max_size.csv")
  )
} else {
  stop("Unsupported pseudo_count: ", pseudo_count)
}
file_y <- file.path(data_dir, paste0("task", data_id, "_preproc_y.csv"))

df_x <- read.csv(file_x)
df_y <- read.csv(file_y) |>
  mutate(Var = as.factor(Var))

## Train-test split of input data ----
id_col <- "sample_id"
merged <- df_x |>
  inner_join(df_y, by = id_col)

split <- split_grouped_stratified(
  merged,
  id_col = id_col,
  outcome_name = "Var",
  prop = 0.8,
  seed = split_seed
)

train_df <- split$train
test_df <- split$test

train_x <- train_df |> select(-outcome)
train_y <- train_df |> select(outcome)

## Model configs ----
model_seed_val <- 2025
eval_metrics <- metric_set(accuracy, roc_auc, brier_class)
rf_trees <- 500
xgb_trees <- 100
lasso_loss <- "deviance" # or "auc"

## Helper: run all models on given train/test and save results as RDS
save_results <- function(train_set, test_set, aug_factor_now) {
  models <- c("lasso", "random_forest", "xgboost", "lasso_ilr")

  for (m in models) {
    if (m == "lasso_ilr") {
      # Transform both train and test via standard ILR on predictors
      x_tr_ilr <- train_set |> select(-outcome) |> ilr()
      colnames(x_tr_ilr) <- paste0("V", seq_len(ncol(x_tr_ilr)))
      train_use <- as.data.frame(x_tr_ilr) |>
        bind_cols(outcome = train_set$outcome)

      x_te_ilr <- test_set |> select(-outcome) |> ilr()
      colnames(x_te_ilr) <- paste0("V", seq_len(ncol(x_te_ilr)))
      test_use <- as.data.frame(x_te_ilr) |>
        bind_cols(outcome = test_set$outcome)

      res <- train_evaluate_one(
        train_data = train_use,
        test_data = test_use,
        model = "lasso",
        loss = lasso_loss,
        model_seed = model_seed_val,
        my_metrics = eval_metrics
      )
      add_param <- lasso_loss
    } else if (m == "lasso") {
      res <- train_evaluate_one(
        train_data = train_set,
        test_data = test_set,
        model = "lasso",
        loss = lasso_loss,
        model_seed = model_seed_val,
        my_metrics = eval_metrics
      )
      add_param <- lasso_loss
    } else if (m == "random_forest") {
      res <- train_evaluate_one(
        train_data = train_set,
        test_data = test_set,
        model = "random_forest",
        trees = rf_trees,
        model_seed = model_seed_val,
        my_metrics = eval_metrics
      )
      add_param <- rf_trees
    } else if (m == "xgboost") {
      res <- train_evaluate_one(
        train_data = train_set,
        test_data = test_set,
        model = "xgboost",
        trees = xgb_trees,
        model_seed = model_seed_val,
        my_metrics = eval_metrics
      )
      add_param <- xgb_trees
    }

    results <- tibble(
      data_id = data_id,
      split = split_seed,
      model = m,
      augmentation = aug_strategy,
      augmentation_factor = aug_factor_now,
      perf_metrics = list(res$perf_metrics),
      roc_curve = list(res$roc_curve),
      add_params = list(add_param),
      pseudo_count = pseudo_count
    )

    out_file <- file.path(
      out_dir,
      paste0(
        "res_tsk",
        data_id,
        "_",
        aug_strategy,
        "_mod_",
        m,
        "_augfac_",
        aug_factor_now,
        "_split_",
        split_seed,
        "_pc_",
        pseudo_count
      )
    )
    saveRDS(results, out_file)
  }
}

## Iterate over augmentation factors: max_factor:2 ----
for (f in seq(from = max_factor, to = 2, by = -1)) {
  # Augment training set using selected Rodriguez method
  set.seed(split_seed)
  train_aug <- aug_data_rodriguez(
    x_data = train_x,
    y_data = train_y,
    multiplier = f,
    aug_strategy = aug_strategy
  )

  # Ensure `outcome` column exists
  if (!"outcome" %in% names(train_aug)) {
    stop(
      "aug_data_rodriguez() must return a data frame with an 'outcome' column."
    )
  }

  save_results(train_set = train_aug, test_set = test_df, aug_factor_now = f)
}

message(
  "Done. Saved results for augmentation factors: ",
  paste(seq(from = max_factor, to = 2, by = -1), collapse = ", ")
)

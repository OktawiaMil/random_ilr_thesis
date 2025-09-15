## Random ILR pipeline
## - Builds augmented data for max_factor
## - Trains all models (lasso, random_forest, xgboost)
## - Iteratively reduces augmentation to factors: (max_factor-1) : 2
## - For aug_in_n: row downsampling
## - For aug_in_p: column subset per factor
## - Saves results in out_dir

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

helpers_file <- file.path(repo_root, "R", "01b_random_ilr_helpers.R")
train_eval_file <- file.path(repo_root, "R", "01_helper_functions.R")
if (file.exists(helpers_file)) {
  source(helpers_file)
}
if (file.exists(train_eval_file)) {
  source(train_eval_file)
}

## Argument parsing ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 8) {
  stop(
    paste0(
      "Expected 8 args: data_dir data_id aug_strategy split_seed max_factor 
      skew_sym_density pseudo_count out_dir\n",
      "Got ",
      length(args),
      ": ",
      paste(args, collapse = " ")
    )
  )
}

data_dir <- args[1]
data_id <- args[2]
aug_strategy <- args[3] # one of: aug_in_n, aug_in_p
split_seed <- as.integer(args[4])
max_factor <- as.integer(args[5])
skew_sym_density <- args[6]
pseudo_count <- args[7]
out_dir <- args[8]

if (is.na(max_factor) || max_factor < 2) {
  stop("max_factor must be an integer >= 2")
}

skew_sym_density_num <- if (identical(skew_sym_density, "NA")) {
  NA_real_
} else {
  as.numeric(skew_sym_density)
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

## Build augmented dataset at max_factor (no split) ----
set.seed(split_seed)
aug_full <- build_augmented_randomilr_full(
  x_data = df_x,
  y_data = df_y,
  multiplier = max_factor,
  aug_strategy = aug_strategy,
  outcome_name = "Var",
  id_col = "sample_id",
  include_standard_ilr = TRUE,
  standardize = TRUE,
  density = skew_sym_density_num
)

## Create grouped+stratified split for max_factor ----
split_max <- split_grouped_stratified(
  df = aug_full,
  id_col = "sample_id",
  outcome_name = "Var",
  prop = 0.8,
  seed = split_seed
)
train_full <- split_max$train
test_full <- split_max$test

## Model configs ----
model_seed_val <- 2025
eval_metrics <- metric_set(accuracy, roc_auc, brier_class)
rf_trees <- 500
xgb_trees <- 100
lasso_loss <- "deviance" # or "auc"

## Helper: run all models on given train/test and save results as RDS
save_results <- function(train_df, test_df, aug_factor_now) {
  models <- c("lasso", "random_forest", "xgboost")
  dens_str <- ifelse(
    is.na(skew_sym_density_num),
    "def",
    str_replace(as.character(skew_sym_density), "\\.", "_")
  )

  for (m in models) {
    if (m == "lasso") {
      res <- train_evaluate_one(
        train_data = train_df,
        test_data = test_df,
        model = "lasso",
        loss = lasso_loss,
        model_seed = model_seed_val,
        my_metrics = eval_metrics
      )
      add_param <- lasso_loss
    } else if (m == "random_forest") {
      res <- train_evaluate_one(
        train_data = train_df,
        test_data = test_df,
        model = "random_forest",
        trees = rf_trees,
        model_seed = model_seed_val,
        my_metrics = eval_metrics
      )
      add_param <- rf_trees
    } else if (m == "xgboost") {
      res <- train_evaluate_one(
        train_data = train_df,
        test_data = test_df,
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
      density = as.numeric(skew_sym_density_num),
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
        "_den_",
        dens_str,
        "_split_",
        split_seed,
        "_pc_",
        pseudo_count
      )
    )
    saveRDS(results, out_file)
  }
}

## Train on max_factor ----
save_results(train_full, test_full, aug_factor_now = max_factor)

## Iteratively reduce augmentation factor from  max_factor-1 to 2 ----
# Below loop creates augmented datasets for aug_factors: (max_factor-1):2
# For aug_in_n: keep the first n * f rows of aug_full (nested transforms)
# For aug_in_p: keep the first K predictors consistently across splits
# Code is executed for max_factor > 2 (otherwise it doesn't make sense)
if (max_factor > 2) {
  for (f in seq(from = max_factor - 1, to = 2, by = -1)) {
    if (aug_strategy == "aug_in_n") {
      # Nested rows: take first n0 * f rows from the max dataset
      n0 <- nrow(df_y)
      take_n <- n0 * f
      aug_f <- aug_full[seq_len(take_n), , drop = FALSE]
      # Split grouped+stratified for this factor
      split_f <- split_grouped_stratified(
        df = aug_f,
        id_col = "sample_id",
        outcome_name = "Var",
        prop = 0.8,
        seed = split_seed
      )
      train_f <- split_f$train
      test_f <- split_f$test
    } else if (aug_strategy == "aug_in_p") {
      # Number of predictors in dataset augmented by max_factor:
      predictors_full <- setdiff(names(aug_full), c("sample_id", "Var"))
      total_pred_cols <- length(predictors_full)
      # Number of columns to sample for factor f:
      k_cols <- floor(total_pred_cols * f / max_factor)
      aug_f <- select_first_k_predictors(
        aug_full,
        k_cols,
        outcome_col = "Var"
      )
      # Split grouped+stratified for this factor
      split_f <- split_grouped_stratified(
        df = aug_f,
        id_col = "sample_id",
        outcome_name = "Var",
        prop = 0.8,
        seed = split_seed
      )
      train_f <- split_f$train
      test_f <- split_f$test
    } else {
      stop("Unsupported aug_strategy: ", aug_strategy)
    }
    # Train models & save it results
    save_results(train_f, test_f, aug_factor_now = f)
  }
}

# If max_factor == 2 -> computation was done only for 1 factor
# Make the correct printing of used factors
factors_used <- if (max_factor > 2) {
  seq(max_factor, 2L)
} else {
  max_factor
}
message(
  "Done. Saved results for augmentation factors: ",
  paste(factors_used, collapse = ", ")
)

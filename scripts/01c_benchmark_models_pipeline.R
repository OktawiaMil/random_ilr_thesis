## Benchmark models for both random ILR and Rodriguez methods

## Libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidymodels)
  library(glmnet)
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

train_eval_file <- file.path(repo_root, "R", "01_helper_functions.R")
if (file.exists(train_eval_file)) {
  source(train_eval_file)
}

## Argument parsing ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 6) {
  stop(
    paste0(
      "Expected 6 args: data_dir data_id transform split_seed 
       pseudo_count out_dir\n",
      "Got ",
      length(args),
      ": ",
      paste(args, collapse = " ")
    )
  )
}

data_dir <- args[1]
data_id <- args[2]
transform <- args[3] # one of: standard_ilr, proportion
split_seed <- as.integer(args[4])
pseudo_count <- args[5]
out_dir <- args[6]

cat(
  "Running benchmark with data_dir=",
  data_dir,
  " data_id=",
  data_id,
  " transform=",
  transform,
  " split_seed=",
  split_seed,
  " pseudo_count=",
  pseudo_count,
  "\n",
  sep = ""
)

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

# Prepare data ----
set.seed(split_seed)

id_col <- "sample_id"
outcome_col <- "Var"

if (transform == "standard_ilr") {
  predictors <- df_x |> select(-all_of(id_col))
  ilr_tbl <- predictors |> ilr() |> as_tibble()
  colnames(ilr_tbl) <- paste0("ilr_", seq_len(ncol(ilr_tbl)))
  merged <- df_x |>
    select(all_of(id_col)) |>
    bind_cols(ilr_tbl) |>
    inner_join(df_y, by = id_col) |>
    arrange(across(all_of(id_col)))
} else if (transform == "proportion") {
  predictors <- df_x |> select(-all_of(id_col))
  row_sums <- rowSums(predictors)
  if (!all(abs(row_sums - 1) < 1e-8)) {
    predictors <- predictors / row_sums
  }
  merged <- df_x |>
    select(all_of(id_col)) |>
    bind_cols(as_tibble(predictors)) |>
    inner_join(df_y, by = id_col) |>
    arrange(across(all_of(id_col)))
} else {
  stop("Unsupported transform: ", transform)
}

split_res <- split_grouped_stratified(
  df = merged,
  id_col = id_col,
  outcome_name = outcome_col,
  prop = 0.8,
  seed = split_seed
)

train_full <- split_res$train |> mutate(outcome = as.factor(outcome))
test_full <- split_res$test |> mutate(outcome = as.factor(outcome))

## Model configs ----
model_seed_val <- 2025
eval_metrics <- metric_set(accuracy, roc_auc, brier_class)
rf_trees <- 500
xgb_trees <- 100
lasso_loss <- "deviance" # or "auc"

## Helper: run all models on given train/test and save results as RDS
save_results <- function(train_df, test_df) {
  models <- c("lasso", "random_forest", "xgboost")

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
      transform = transform,
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
        "_mod_",
        m,
        "_tr_",
        transform,
        "_split_",
        split_seed,
        "_pc_",
        pseudo_count
      )
    )
    saveRDS(results, out_file)
  }
}

## Train models ----
save_results(train_full, test_full)
message(
  "Done. Saved results for benchmark models."
)

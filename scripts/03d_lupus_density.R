# Train standard log-contrast regression, XGB, RF models on original  vs.
# data augmented in p (random ILR) vs. one of Rodriguez methds vs.
# augmented by both random ILR + Rodriguez
# %%
# install.packages("devtools")
# devtools::install_github("bio-datascience/augmentR")
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidymodels)
  library(glmnet)
  library(augmentR)
  library(compositions)
  library(here)
  library(future)
  library(future.apply)
  library(trac)
})

# Source helper functions
helpers_file <- file.path(here::here(), "R", "03_helper_functions.R")
if (file.exists(helpers_file)) {
  source(helpers_file)
}

# Read in data
data_dir <- here::here("data", "data_lupus", "data_preproc")
data_lupus <- readRDS(file.path(data_dir, "data_lupus_prep.RDS"))

# TODO: change back to the correct output dir
# output_dir <- here::here("results", "lupus_results")
output_dir <- here::here("results", "lupus_density_results")

#dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Create df with necessary data
lupus_df <- data_lupus$prop_pc |>
  bind_cols(outcome = data_lupus$y, data_lupus$covariates |> select(id))

#%% Config
#TODO : change for the values you really want to have there
model_seed <- 2025
aug_factor <- 3
sel_dens_vec <- c(NA, seq(0.05, 0.5, by = 0.05))
rf_trees <- 500
xgb_trees <- 100
split_seeds <- 1:20

# Wrapper for a single split seed
run_one_split <- function(split_seed, sel_density) {
  set.seed(split_seed)
  split <- group_initial_split(
    lupus_df,
    group = id,
    strata = outcome,
    prop = 0.8
  )

  # Get directly train - test data - used in the benchmark and Rodriguez
  train_data <- training(split) |> select(-id)
  test_data <- testing(split) |> select(-id)

  # Get the indicies of both sets - needed in the aug_in_p
  train_idx <- split$in_id
  test_idx <- setdiff(seq_len(nrow(lupus_df)), train_idx)

  # Benchmark model fit
  # ILR transformation
  train_df_ilr <- train_data |>
    select(-outcome) |>
    ilr() |>
    as.data.frame() |>
    bind_cols(train_data |> select(outcome))

  test_df_ilr <- test_data |>
    select(-outcome) |>
    ilr() |>
    as.data.frame() |>
    bind_cols(test_data |> select(outcome))

  fit_and_save_one_split(
    train_df = train_df_ilr,
    test_df = test_df_ilr,
    split_seed = split_seed,
    output_dir = output_dir,
    train_idx = train_idx,
    test_idx = test_idx
  )

  # Fit and save sparse log contrast benchmark model
  # Sparse log contrast benchmark is fitted on log(X + PC)
  data_bench_slc <- data_lupus$log_pc_max_lib_size |>
    bind_cols(outcome = data_lupus$y)

  # Train/test split
  train_bench_slc <- data_bench_slc[train_idx, ]
  test_bench_slc <- data_bench_slc[test_idx, ]

  sparse_log_cont_custom(
    train_data = train_bench_slc,
    test_data = test_bench_slc,
    split_seed = split_seed,
    output_dir = output_dir,
    train_idx = train_idx,
    test_idx = test_idx
  )

  # Rodriguez augmentation method
  x_tr <- train_data |>
    select(-outcome)
  y_tr <- train_data |>
    select(outcome)

  data_aug_rod <- aitchison_mixup(
    x_data = x_tr,
    y_data = y_tr,
    multiplier = aug_factor
  )

  # Augmented train dataset transform with ilr
  aug_rod_x <- data_aug_rod |>
    select(-outcome) |>
    ilr() |>
    as.data.frame()

  data_aug_rod_ilr <- aug_rod_x |>
    bind_cols(
      data_aug_rod |>
        select(outcome)
    )

  # Fit models on Rodriguez augmented data
  fit_and_save_one_split(
    train_df = data_aug_rod_ilr,
    test_df = test_df_ilr,
    split_seed = split_seed,
    output_dir = output_dir,
    train_idx = train_idx,
    test_idx = test_idx,
    aug_strategy = "aitchison_mixup",
    aug_factor = aug_factor
  )

  # Random ILR
  # Prepare x and y data (whole dataset)
  x_data <- lupus_df |> select(-all_of(c("outcome", "id")))
  y_data <- lupus_df |> select(outcome)

  # Augment whole datset
  aug_p_data <- aug_p_randilr(
    x_data = x_data,
    y_data = y_data,
    multiplier = aug_factor,
    density = sel_density
  )
  # Get train and test data
  aug_p_train <- aug_p_data[train_idx, ]
  aug_p_test <- aug_p_data[test_idx, ]

  # Fit and save models on aug_in_p augmented data
  fit_and_save_one_split(
    train_df = aug_p_train,
    test_df = aug_p_test,
    split_seed = split_seed,
    output_dir = output_dir,
    train_idx = train_idx,
    test_idx = test_idx,
    aug_strategy = paste0("aug_in_p_dens_", sel_density),
    aug_factor = aug_factor
  )

  # 2 stage augmentation
  # 1. Aitchison on train set - we reuse already augmented data (data_aug_rod)
  # 2. augment in p
  # Combine back together augmented train and test but add column specyfying
  # whether a sample is in test or train set
  data_stage1 <- data_aug_rod |>
    mutate(id = "train") |>
    bind_rows(
      test_data |>
        mutate(id = "test")
    )

  data_st1_x <- data_stage1 |> select(-outcome)
  data_st1_y <- data_stage1 |> select(outcome, id)

  # Augment in p
  data_aug_p <- aug_p_randilr(
    x_data = data_st1_x,
    y_data = data_st1_y,
    id_col = "id",
    density = sel_density,
    multiplier = aug_factor,
    id_action = "keep"
  )

  # Split into train and test again
  train_stage2 <- data_aug_p |> filter(id == "train") |> select(-id)
  test_stage2 <- data_aug_p |> filter(id == "test") |> select(-id)

  fit_and_save_one_split(
    train_df = train_stage2,
    test_df = test_stage2,
    split_seed = split_seed,
    output_dir = output_dir,
    train_idx = train_idx,
    test_idx = test_idx,
    aug_strategy = paste0("aitchison_aug_in_p_dens_", sel_density),
    aug_factor = aug_factor
  )
}

#%%
# Execute workflow for all seeds
py <- reticulate::conda_python("trac")
Sys.setenv(RETICULATE_PYTHON = py)

# Small initializer that runs inside every worker
.worker_init <- function() {
  library(reticulate)
  reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
  if (!reticulate::py_module_available("classo")) {
    stop("Python module 'classo' not found in the 'trac' env")
  }
  # load R packages used inside run_one_split() (workers are fresh R sessions)
  for (p in c(
    "dplyr",
    "stringr",
    "tidymodels",
    "glmnet",
    "augmentR",
    "compositions",
    "here",
    "future",
    "future.apply",
    "trac"
  )) {
    requireNamespace(p, quietly = TRUE)
  }
  invisible(TRUE)
}

grid <- expand.grid(
  seed = split_seeds,
  dens = sel_dens_vec,
  KEEP.OUT.ATTRS = FALSE
)

plan(multisession, workers = 5)
res <- future_lapply(
  seq_len(nrow(grid)),
  function(s) {
    .worker_init()
    run_one_split(grid$seed[s], grid$dens[s])
  },
  future.seed = TRUE
)
future::plan(sequential)

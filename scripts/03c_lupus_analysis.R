# Train standard log-contrast regression, XGB, RF models on original  vs.
# data augmented in p (random ILR) vs. one of Rodriguez methds vs.
# augmented by both random ILR + Rodriguez
# %%
#devtools::install_github("OktawiaMil/augmenter")
suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(tidymodels)
    library(glmnet)
    library(augmenter)
    library(compositions)
    library(here)
    library(future)
    library(future.apply)
})

# Source helper functions
helpers_file <- file.path(here::here(), "R", "03_helper_functions.R")
if (file.exists(helpers_file)) {
    source(helpers_file)
}

# Read in data
data_dir <- here::here("data", "data_lupus", "data_preproc")
data_lupus <- readRDS(file.path(data_dir, "data_lupus_prep.RDS"))
output_dir <- here::here("results", "lupus_results")
#dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Create df with necessary data
lupus_df <- data_lupus$prop_pc |>
    bind_cols(outcome = data_lupus$y, data_lupus$covariates |> select(id))

#%% Config
model_seed <- 2025
aug_factor <- 3
sel_density <- 0.1
rf_trees <- 500
xgb_trees <- 100
split_seeds <- 1:20


# Wrapper for a single split seed
run_one_split <- function(split_seed) {
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

    # # Fit models on Rodriguez augmented data
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
        aug_strategy = "aug_in_p",
        aug_factor = aug_factor
    )

    # Combine augmentation methods - first augment in p
    # Split into train and test - reuse from above
    # Augment train data with Aitchison Mixup -> fit and test model
    # Rodriguez augmentation method
    # aug_p_x <- aug_p_train |>
    #     select(-outcome)
    # aug_p_y <- aug_p_train |>
    #     select(outcome)

    # aug_comb_train <- aitchison_mixup(
    #     x_data = aug_p_x,
    #     y_data = aug_p_y,
    #     multiplier = aug_factor
    # )

    # fit_and_save_one_split(
    #     train_df = aug_comb_train,
    #     test_df = aug_p_test,
    #     split_seed = split_seed,
    #     output_dir = output_dir,
    #     train_idx = train_idx,
    #     test_idx = test_idx,
    #     aug_strategy = "aug_in_p_Aitchison",
    #     aug_factor = aug_factor
    # )
}

#%%
# Execute workflow for all seeds
plan(multisession, workers = parallel::detectCores() - 2)
future_lapply(
    split_seeds,
    run_one_split,
    future.seed = TRUE # reproducible per split
)

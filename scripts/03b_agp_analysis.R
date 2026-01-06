# Train logistic regression with L1 penalty (for benchamrk sparse log contrast
# as well) XGB, RF models on original  vs. data augmented in p (random ILR) vs.
# one of Rodriguez methds vs.
# augmented by both random ILR + Rodriguez
#%%
# Installation instrruction of trac and augmentR
# create environment - needed to install trac
# reticulate::conda_create("trac", packages = "python=3.10") # one-time: creates env 'trac'
# # Install needed libraries in the trac environment
# reticulate::conda_install(
#     "trac",
#     c("c-lasso", "numpy", "scipy", "pandas", "matplotlib"),
#     pip = TRUE
# )
# py <- reticulate::conda_python("trac")
# Sys.setenv(RETICULATE_PYTHON = py)
# reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)
# install.packages("devtools")
# devtools::install_github("viettr/trac")
# devtools::install_github("bio-datascience/augmentR")

# Hard-pin the interpreter
library(reticulate)
Sys.setenv(
    RETICULATE_PYTHON = reticulate::conda_python("trac"),
    RETICULATE_AUTOINSTALL_PYTHON = "FALSE"
)
reticulate::use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE)

suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(tidymodels)
    library(glmnet)
    library(augmentR)
    library(compositions)
    library(trac)
    library(important)
})

# AGP calculations are run on server
## Robust sourcing of helpers from R/ folder
script_args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub(
    "--file=",
    "",
    script_args[grep("^--file=", script_args)]
))
script_dir <- dirname(script_path)
repo_root <- dirname(script_dir)

helpers_file <- file.path(repo_root, "R", "03_helper_functions.R")
if (file.exists(helpers_file)) {
    source(helpers_file)
}

# Parsing arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) {
    stop(
        paste0(
            "Expected 4 args: data_dir split_seed sel_density output_dir\n",
            "Got ",
            length(args),
            ": ",
            paste(args, collapse = " ")
        )
    )
}

data_dir <- args[1]
split_seed <- as.integer(args[2])
sel_density <- as.numeric(args[3])
output_dir <- args[4]

# Read in data
data_agp <- readRDS(file.path(data_dir, "data_agp_prep.rds"))

# Create df with necessary data
agp_df <- data_agp$prop_pc |> bind_cols(outcome = data_agp$y)

#%% Config
model_seed <- 2025
aug_factor <- 3
rf_trees <- 500
xgb_trees <- 100

# Wrapper for a single split seed
run_one_split <- function(split_seed) {
    # List to store ilr matrices
    ilr_list <- list()

    # Train-test split
    set.seed(split_seed)
    split <- initial_split(
        agp_df,
        strata = "outcome",
        prop = 0.8
    )
    # Get directly train - test data - used in the benchmark and Rodriguez
    train_data <- training(split)
    test_data <- testing(split)

    # Get the indices of both sets - needed in the aug_in_p
    train_idx <- split$in_id
    test_idx <- setdiff(seq_len(nrow(agp_df)), train_idx)

    # Benchmark model fit
    # ILR transformation
    train_ilr <- train_data |>
        select(-outcome) |>
        ilr()

    train_df_ilr <- train_ilr |>
        as.data.frame() |>
        bind_cols(train_data |> select(outcome))

    # Save ILR matrix - needed to decode important predictors
    # The same ILR matrix used in benchmark and Rodriguez aug. method
    ilr_list$standard_ilr <- ilrBase(z = train_ilr)

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
    data_bench_slc <- data_agp$log_pc_max_lib_size |>
        bind_cols(outcome = data_agp$y)

    # Train/test split
    train_bench_slc <- data_bench_slc[train_idx, ]
    test_bench_slc <- data_bench_slc[test_idx, ]

    # Fit and save sparse log contrast benchmark model
    sparse_log_cont_custom(
        train_data = train_bench_slc,
        test_data = test_bench_slc,
        split_seed = split_seed,
        output_dir = output_dir,
        train_idx = train_idx,
        test_idx = test_idx,
        min_frac = 6e-03 # I change this parameter because in
        # my experiance the default setting leads to fitting many empty
        # models & increases runtime
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
    x_data <- agp_df |> select(-outcome)
    y_data <- agp_df |> select(outcome)

    # Augment whole datset
    aug_p_data <- aug_p_randilr(
        x_data = x_data,
        y_data = y_data,
        multiplier = aug_factor,
        density = sel_density,
        return_ilr_bases = TRUE
    )
    # Get train and test data
    aug_p_train <- aug_p_data$data_aug[train_idx, ]
    aug_p_test <- aug_p_data$data_aug[test_idx, ]

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

    # Save ilr matrix
    ilr_list$aug_p <- aug_p_data$ilr_base

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
        id_action = "keep",
        return_ilr_bases = TRUE
    )

    # Split into train and test again
    train_stage2 <- data_aug_p$data_aug |> filter(id == "train") |> select(-id)
    test_stage2 <- data_aug_p$data_aug |> filter(id == "test") |> select(-id)

    fit_and_save_one_split(
        train_df = train_stage2,
        test_df = test_stage2,
        split_seed = split_seed,
        output_dir = output_dir,
        train_idx = train_idx,
        test_idx = test_idx,
        aug_strategy = "aitchison_aug_in_p",
        aug_factor = aug_factor
    )

    # Save ilr matrix
    ilr_list$aitchison_aug_p <- data_aug_p$ilr_base

    # Save list with ILR matricies
    saveRDS(
        ilr_list,
        file.path(output_dir, paste0("ilr_basis_split_", split_seed, ".rds"))
    )
}

# Server:
run_one_split(split_seed)

## Aggregate results across datasets, splits, pseudo-counts, and augmentation
## factors for Rodriguez methods,random-ILR methods and benchmarks.
## Output: one file per model type x augmentation strategy/benchmark.

## Libraries
library(purrr)
library(dplyr)

dir_root <- here::here()
dir_out <- file.path(dir_root, "results", "results_aggregated")

# %%
# Rodriguez augmentation methods ----
dir_res <- file.path(dir_root, "results", "rodriguez")
models_rod <- c("lasso", "lasso_ilr", "random_forest", "xgboost")

for (sel_model in models_rod) {
  for (pseudo_count in c("half", "max_lib_size")) {
    pattern <- glue::glue(
      "^res_tsk[0-9]+_[A-Za-z]+_[A-Za-z]+_mod_{sel_model}_augfac_[0-9]+_split_[0-9]+_pc_{pseudo_count}$"
    )
    # List all files from dir_res that follow specified pattern
    files <- list.files(dir_res, pattern = pattern, full.names = TRUE)

    # Read in and merge all files into 1 tibble:
    all_results <- map(files, readRDS) %>%
      bind_rows()

    # Save merged results
    file_name <- glue::glue("rodriguez_{sel_model}_pc_{pseudo_count}")
    saveRDS(all_results, file.path(dir_out, file_name))
    rm(all_results)
  }
}

# %%
## Random ILR methods ----
dir_res <- file.path(dir_root, "results", "random_ilr")
models_ilr <- c("lasso", "random_forest", "xgboost")

for (sel_model in models_ilr) {
  for (pseudo_count in c("half", "max_lib_size")) {
    pattern <- glue::glue(
      "^res_tsk[0-9]+_(?:aug_in_n|aug_in_p)_mod_{sel_model}_augfac_[0-9]+_den_.+?_split_[0-9]+_pc_{pseudo_count}$"
    )
    # List all files from dir_res that follow specified pattern
    files <- list.files(dir_res, pattern = pattern, full.names = TRUE)

    # Read in and merge all files into 1 tibble:
    all_results <- map(files, readRDS) %>%
      bind_rows()

    # Save merged results
    file_name <- glue::glue("random_ilr_{sel_model}_pc_{pseudo_count}")
    saveRDS(all_results, file.path(dir_out, file_name))
    rm(all_results)
  }
}

# %%
## Benchmark models ----
dir_res <- file.path(dir_root, "results", "benchmark")
models <- c("lasso", "random_forest", "xgboost")

for (sel_model in models) {
  for (pseudo_count in c("half", "max_lib_size")) {
    pattern <- glue::glue(
      "^res_tsk[0-9]+_mod_{sel_model}_tr_(?:standard_ilr|proportion)_split_[0-9]+_pc_{pseudo_count}$"
    )
    # List all files from dir_res that follow specified pattern
    files <- list.files(dir_res, pattern = pattern, full.names = TRUE)

    # Read in and merge all files into 1 tibble:
    all_results <- map(files, readRDS) %>%
      bind_rows()

    # Save merged results
    file_name <- glue::glue("benchmark_{sel_model}_pc_{pseudo_count}")
    saveRDS(all_results, file.path(dir_out, file_name))
    rm(all_results)
  }
}
# %%

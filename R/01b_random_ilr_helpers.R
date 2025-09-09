library(dplyr)
library(augmenter)
library(rsample)
library(rlang)

# For aug_in_p: keep first K predictors (columns) consistently for train/test
# df - augmented dataset
# k - number of columns to select from df
# outcome_col - name of the column with the outcome variable
select_first_k_predictors <- function(df, k, outcome_col = "outcome") {
  predictors <- setdiff(names(df), outcome_col)
  k <- max(1L, min(length(predictors), as.integer(k)))
  keep <- c(predictors[seq_len(k)], outcome_col)
  df[, keep, drop = FALSE]
}

# Build augmented dataset at selected augmentation factor without splitting
# Ensures nested ordering for aug_in_n by arranging rows in blocks of size n
# so that the first n rows correspond to the first transform across all IDs,
# next n rows to the second transform, etc.
build_augmented_randomilr_full <- function(
  x_data,
  y_data,
  multiplier = 2,
  aug_strategy = c("aug_in_n", "aug_in_p"),
  outcome_name = "Var",
  id_col = NULL,
  include_standard_ilr = TRUE,
  standardize = TRUE,
  density = NA
) {
  aug_strategy <- match.arg(aug_strategy)

  if (aug_strategy == "aug_in_n") {
    df <- aug_n_randilr(
      x_data = x_data,
      y_data = y_data,
      multiplier = multiplier,
      include_standard_ilr = include_standard_ilr,
      standardize = standardize,
      id_col = id_col,
      density = density
    ) %>%
      arrange(across(all_of(id_col)))

    # Create within-ID index to impose nested row blocks
    df <- df %>%
      group_by(across(all_of(id_col))) %>%
      mutate(.rep_idx = row_number()) %>%
      ungroup() %>%
      arrange(.rep_idx, across(all_of(id_col))) %>%
      select(-.rep_idx)
  } else {
    df <- aug_p_randilr(
      x_data = x_data,
      y_data = y_data,
      id_col = id_col,
      multiplier = multiplier,
      density = density
    ) %>%
      arrange(across(all_of(id_col)))
  }

  df
}

# Grouped + stratified split helper for a prepared dataset
split_grouped_stratified <- function(
  df,
  id_col,
  outcome_name,
  prop = 0.8,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  split <- group_initial_split(
    df,
    group = !!sym(id_col),
    strata = !!sym(outcome_name),
    prop = prop
  )

  train <- training(split) %>%
    dplyr::select(-all_of(id_col)) %>%
    dplyr::rename(outcome = all_of(outcome_name))

  test <- testing(split) %>%
    dplyr::select(-all_of(id_col)) %>%
    dplyr::rename(outcome = all_of(outcome_name))

  list(train = train, test = test)
}

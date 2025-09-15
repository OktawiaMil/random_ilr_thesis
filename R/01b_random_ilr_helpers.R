library(dplyr)
library(augmenter)
library(rsample)
library(rlang)

#' Select first K predictors for aug_in_p
#'
#' Keeps the first `k` predictor columns (in current column order) and the
#' `outcome_col` column. Useful for building nested feature subsets for
#' the "aug_in_p" strategy so that smaller factors are subsets of larger ones.
#'
#' @param df Data frame with predictors and an outcome column.
#' @param k Integer; number of predictor columns to keep.
#' @param outcome_col Character; name of the outcome column. Defaults to
#'   "outcome".
#'
#' @return A data frame with `k` predictors plus the outcome column.
#'
#' @examples NULL
select_first_k_predictors <- function(df, k, outcome_col = "outcome") {
  predictors <- setdiff(names(df), outcome_col)
  k <- max(1L, min(length(predictors), as.integer(k)))
  keep <- c(predictors[seq_len(k)], outcome_col)
  df[, keep, drop = FALSE]
}

#' Build full augmented dataset (no split)
#'
#' Builds the augmented dataset for the selected random ILR strategy at the
#' specified `multiplier` without performing a train/test split. For
#' `aug_in_n`, the output rows are ordered in nested blocks across IDs so that
#' the first `n` rows are transform 1 (e.g., standard ILR if included) for all
#' IDs, the next `n` rows are transform 2, etc. This enables deterministic
#' nested slicing by factor.
#'
#' @param x_data A data.frame of compositional predictors.
#' @param y_data A data.frame with the outcome and ID.
#' @param multiplier Integer; augmentation factor.
#' @param aug_strategy Character; one of "aug_in_n" or "aug_in_p".
#' @param outcome_name Character; name of the outcome column.
#' @param id_col Character or NULL; ID column name.
#' @param include_standard_ilr Logical; include standard ILR
#' transform (aug_in_n).
#' @param standardize Logical; standardize ILR features before
#' merging (aug_in_n).
#' @param density Numeric in (0,1] or NA; density for GHL matrix generation.
#'
#' @return A data frame with ID and outcome columns preserved.
#'
#' @examples NULL
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
    ) |>
      arrange(across(any_of(id_col)))

    # Create within-ID index to impose nested row blocks
    df <- df |>
      group_by(across(any_of(id_col))) |>
      mutate(.rep_idx = row_number()) |>
      ungroup() |>
      arrange(.rep_idx, across(any_of(id_col))) |>
      select(-.rep_idx)
  } else {
    df <- aug_p_randilr(
      x_data = x_data,
      y_data = y_data,
      id_col = id_col,
      multiplier = multiplier,
      density = density
    ) |>
      arrange(across(any_of(id_col)))
  }

  df
}

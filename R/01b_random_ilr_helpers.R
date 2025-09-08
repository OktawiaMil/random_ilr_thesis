library(dplyr)
library(augmenter)
library(rsample)
library(rlang)

#' Augment dataset using one of random ILR strategy
#'
#' Creates an augmented dataset using the random ILR (isometric log-ratio)
#' method in either the rows ("aug_in_n") or the features ("aug_in_p"),
#' then performs a grouped, stratified train/test split.
#'
#' @param x_data A data.frame of compositional predictors.
#' @param y_data A data.frame with the outcome (may also include ID).
#' @param multiplier Integer that controls the number of ILR-transformed
#'   versions of the input data.
#' @param aug_strategy Character; one of "aug_in_n" or "aug_in_p".
#' @param outcome_name Character; name of the outcome column in `y_data`.
#' @param id_col Character; column name(s) with subjects ID.
#' @param include_standard_ilr Logical; specifies if standard ILR transform of
#'  original data should be included (only for "aug_in_n").
#' @param standardize Logical; standardize ILR features before merging
#' (only for "aug_in_n").
#' @param density Numeric in (0,1] or NA; density of the skew-symmetric
#'  matrix used to generate the GHL matrix.
#'
#' @return A list with elements `train_data` and `test_data`.
#'
#' @examples NULL
#' @export
aug_dataset_randomilr <- function(
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
    aug_data <- aug_n_randilr(
      x_data = x_data,
      y_data = y_data,
      multiplier = multiplier,
      include_standard_ilr = include_standard_ilr,
      standardize = standardize,
      id_col = id_col,
      density = density
    ) %>%
      arrange(across(all_of(id_col)))
  } else {
    aug_data <- aug_p_randilr(
      x_data = x_data,
      y_data = y_data,
      id_col = id_col,
      multiplier = multiplier,
      density = density
    ) %>%
      arrange(across(all_of(id_col)))
  }

  # Create test - train split
  out_data <- group_initial_split(
    aug_data,
    group = !!sym(id_col),
    strata = !!sym(outcome_name),
    prop = 0.8
  )

  # Extract train and test dataset, return in a list
  train <- training(out_data) %>%
    select(-all_of(id_col)) %>%
    rename(outcome = all_of(outcome_name))

  test <- testing(out_data) %>%
    select(-all_of(id_col)) %>%
    rename(outcome = all_of(outcome_name))

  res <- list(train_data = train, test_data = test)
  return(res)
}

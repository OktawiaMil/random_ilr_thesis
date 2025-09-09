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

# Stratified sampling by outcome to a target size
# Function used to sample train and test datasets fro aug factors < max_factor
# df - augmented dataset
# target_n - target number of rows to select from df
# outcome_col - name of the column with the outcome variable
# seed - numeric, used to set seed to make the rows sampling reproducible
stratified_sample_n <- function(
  df,
  target_n,
  outcome_col = "outcome",
  seed = NULL
) {
  n <- nrow(df)
  if (is.null(target_n) || target_n >= n) {
    return(df)
  }
  target_n <- max(1L, as.integer(target_n))
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Class counts and proportions
  cls <- df[[outcome_col]]
  if (!is.factor(cls)) {
    cls <- factor(cls)
  }
  tab <- table(cls)
  props <- as.numeric(tab) / sum(tab)
  levels_vec <- names(tab)

  # Allocate per class with rounding that preserves total
  raw_targets <- props * target_n
  # Round to integer
  base_targets <- floor(raw_targets)
  # How many samples we still need to hit the goal
  remainder <- target_n - sum(base_targets)
  if (remainder > 0) {
    # Check in which level after rounding down the diff
    # bewteen the number of integer samples and panned samples is the biggest
    frac <- raw_targets - base_targets
    ord <- order(frac, decreasing = TRUE)
    # Give +1 to the top remainder classes:
    base_targets[ord[seq_len(remainder)]] <- base_targets[ord[seq_len(
      remainder
    )]] +
      1L
  }

  # Sample within each class
  out_list <- vector("list", length(levels_vec))
  names(out_list) <- levels_vec
  for (i in seq_along(levels_vec)) {
    lev <- levels_vec[i]
    # Target sample size for the analysed class
    target_i <- base_targets[i]
    # Get rows of df that are in considered class (level)
    grp <- df[df[[outcome_col]] == lev, , drop = FALSE]
    k <- min(nrow(grp), target_i)
    if (k <= 0) {
      out_list[[i]] <- grp[0, , drop = FALSE]
    } else if (k >= nrow(grp)) {
      out_list[[i]] <- grp
    } else {
      idx <- sample.int(nrow(grp), size = k, replace = FALSE)
      out_list[[i]] <- grp[idx, , drop = FALSE]
    }
  }

  bind_rows(out_list)
}

# Helper functions used for data augmentation & model training
# with the use of Rodriguez methods:
# Aitchison Mixup, Compositional feature dropout, Compositional cutmix

library(dplyr)
library(augmenter)
library(rsample)

#' Augment data using onf of Rodriguez methods
#'
#' @param x_data Data frame of compositional features.
#' @param y_data Data frame with outcome.
#' @param aug_strategy One of "aitchison_mixup", "comp_feature", or
#' "comp_cutmix".
#' @param multiplier Numeric, augmentation multiplier passed to the selected
#'  strategy.
#' @return Data frame with augmented dataset.
#' @examples NULL
#' @export
aug_data_rodriguez <- function(
  x_data,
  y_data,
  multiplier = 2,
  aug_strategy = c("aitchison_mixup", "comp_feature", "comp_cutmix")
) {
  aug_strategy <- match.arg(aug_strategy)

  if (aug_strategy == "aitchison_mixup") {
    data_aug <- aitchison_mixup(
      x_data = x_data,
      y_data = y_data,
      multiplier = multiplier
    )
  } else if (aug_strategy == "comp_feature") {
    data_aug <- comp_feat_dropout(
      x_data = x_data,
      y_data = y_data,
      multiplier = multiplier
    )
  } else {
    data_aug <- comp_cutmix(
      x_data = x_data,
      y_data = y_data,
      multiplier = multiplier
    )
  }
  return(data_aug)
}

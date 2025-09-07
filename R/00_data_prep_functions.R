# Packages
library(tibble)
library(dplyr)

#' Create dataset for analysis from raw taxa and task data (from ML Repo)
#'
#' @param taxa_table
#' @param task_table
#'
#' @return Data frame, each row corresponds to one subject, each column
#'   corresponds to one "organism" (no grouping to one taxonomic level)
#' @export
prep_taxa_data <- function(taxa_table, task_table) {
  taxa_table_t <- t(taxa_table[, -1])
  col_names <- taxa_table$`#OTU ID`
  if (sum(is.na(col_names)) > 0) {
    stop("At least one of OTU ID is NA. Fix OTU IDs!")
  }
  colnames(taxa_table_t) <- col_names
  task_x <- taxa_table_t |>
    as.data.frame() |>
    rownames_to_column(var = "sample_id")

  # Filtering patients with known outcome & ordering data
  task_merged <- task_x |>
    inner_join(
      task_table |>
        rename(sample_id = `#SampleID`)
    )
}

#' Function that adds specified pseudo-count to the data and converts it into proportions
pseudo_count_proportion <- function(data, pseudo_count = 1 / 2) {
  if (!is.numeric(pseudo_count)) {
    stop("Argument `pseudo_count` must be numeric.")
  }
  data <- data + pseudo_count
  # Convert into proportions:
  data_prop <- data / rowSums(data)
  return(data_prop)
}

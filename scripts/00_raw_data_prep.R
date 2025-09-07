# The purpose of this script is to prepare all datasets for the supervised part of the analysis

# Libraries
library(here)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# Load helper functions
source(here::here("R", "00_data_prep_functions.R"))

# Directories
# Raw data
directory_raw <- here::here("data", "data_raw")
if (!dir.exists(directory_raw)) {
  dir.create(directory_raw, recursive = TRUE)
}
# Preprocessed data
directory_preproc <- here::here("data", "data_preproc")
if (!dir.exists(directory_preproc)) {
  dir.create(directory_preproc, recursive = TRUE)
}

# Assign correct name to each file
# If a file has in its name "taxatable" - this dataset has only predictors - name it with _x
name_from_file <- function(f) {
  m <- stringr::str_match(basename(f), "^task(\\d+)_(taxatable|task)")
  paste0("task", m[, 2], ifelse(m[, 3] == "taxatable", "_x", "_y"))
}

# List and read all matching txt files
files <- list.files(
  directory_raw,
  pattern = "^task[0-9]+_(taxatable|task)\\.txt$",
  full.names = TRUE
)

purrr::walk(files, function(f) {
  obj_name <- name_from_file(f)
  data_obj <- read_delim(f, show_col_types = FALSE)
  assign(obj_name, data_obj, envir = .GlobalEnv)
})

# Helper: create pseudo-count variant of dataset and attach to it sample_id
pc_variants <- function(predictors, sample_ids) {
  max_lib <- max(rowSums(predictors))
  list(
    pc_max_size = pseudo_count_proportion(
      predictors,
      pseudo_count = 1 / max_lib
    ) |>
      bind_cols(sample_id = sample_ids),
    pc_half = pseudo_count_proportion(predictors, pseudo_count = 1 / 2) |>
      bind_cols(sample_id = sample_ids)
  )
}

# Process one task at a time
process_task <- function(id, drop_extra = character(), fix_otu = FALSE) {
  # Read in x and y data
  x <- get(paste0("task", id, "_x"), envir = .GlobalEnv)
  y <- get(paste0("task", id, "_y"), envir = .GlobalEnv)

  # Fixing OTU ID - if one OTU is NA - set to unknown
  if (fix_otu) {
    x <- x |>
      mutate(`#OTU ID` = if_else(is.na(`#OTU ID`), "unknown", `#OTU ID`))
  }

  # Prepare data for analysis (code in R/)
  dat <- prep_taxa_data(taxa_table = x, task_table = y)

  # Prepare predictors - remove outcome variable and id (and optional extras)
  preds <- dat %>% select(-any_of(c("Var", "sample_id", drop_extra)))

  # Compute both pseudo-count versions and bind sample_id
  pcs <- pc_variants(preds, dat$sample_id)

  # Write X data
  # With pseudo-count == 1/max library size
  readr::write_csv(
    pcs$pc_max_size,
    file.path(
      directory_preproc,
      paste0("task", id, "_preproc_x_pc_max_size.csv")
    )
  )
  # With pseudo-count == 1/2
  readr::write_csv(
    pcs$pc_half,
    file.path(directory_preproc, paste0("task", id, "_preproc_x_pc_half.csv"))
  )

  # Write outcome data
  readr::write_csv(
    dat %>% select(sample_id, Var),
    file.path(directory_preproc, paste0("task", id, "_preproc_y.csv"))
  )
}

# ControlVar present in tasks 3, 5, 6, 7 - remove them when forming predictors set
tasks_with_controlvar <- c(3, 5, 6, 7)

# OTU ID fix needed in these tasks
tasks_with_fix_otu <- 8:12

# Run all tasks
for (id in 1:12) {
  drop_extra <- if (id %in% tasks_with_controlvar) "ControlVar" else character()
  fix_otu <- id %in% tasks_with_fix_otu
  process_task(id, drop_extra = drop_extra, fix_otu = fix_otu)
}

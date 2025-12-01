# File to prepera 2 datasets: American Gut Project and Lupus datasets
# for the 03b and 03c scripts
# Data received - absolute abundance with added PC = 1 and log transformed
#%%
# Libraries
library(here)
library(tidyr)

# Read in datasets
## AGP
data_dir_agp <- here::here("data", "data_agp", "data_raw")
data_agp <- readRDS(file.path(data_dir_agp, "agp_data.RDS"))

## Lupus
data_dir_lupus <- here::here("data", "data_lupus", "data_raw")
data_lupus <- readRDS(file.path(data_dir_lupus, "lupus_data.RDS"))
# Retrive patient ID - needed for groupped train-test split
data_lupus$covariates <- data_lupus$covariates |>
    separate_wider_delim(
        ind_cov_batch_cov,
        delim = ":",
        names = c("id", "other")
    )

# Directories to save preprocessed data
data_preproc_agp <- here::here("data", "data_agp", "data_preproc")
data_preproc_lupus <- here::here("data", "data_lupus", "data_preproc")

# Function that:
# 1. reverts back the log-transformed counts with added pseudo-count == 1
# to absolute abundance counts (exp() and substracts PC = 1)
# 2. adds pseudo-count == 1/max library size
# 3. transforms into proportions
# 4. transforms outcome to factor
# Inputs: data - list with at least two elements - log_count and y
preproc_data <- function(data) {
    abs_abundance <- exp(data$log_count) - 1
    # Calculate PC == 1/max lib size
    pseudo_count <- 1 / max(rowSums(abs_abundance))
    abs_abundance <- abs_abundance + pseudo_count
    # Convert into proportions:
    data_prop <- abs_abundance / rowSums(abs_abundance)
    data$prop_pc <- data_prop
    # Needed for the sparse log-contrast model: log(observed abundance + PC)
    # For the consistency, I add PC = 1/max lib size
    data_log_pc_mls <- log(abs_abundance + pseudo_count)
    data$log_pc_max_lib_size <- data_log_pc_mls
    data$y <- factor(data$y, levels = c(-1, 1))
    return(data)
}

# Preprocess data
data_agp_prep <- preproc_data(data_agp)
data_lupus_prep <- preproc_data(data_lupus)

# Save preprocessed data
saveRDS(data_agp_prep, file.path(data_preproc_agp, "data_agp_prep.rds"))
saveRDS(data_lupus_prep, file.path(data_preproc_lupus, "data_lupus_prep.rds"))

# Helper functions for the summary report of AGP and Lupus data analyses
# Function that reads in results from different splits and merges together into 1 object per model
read_results <- function(dir_res) {
    models <- c(
        lasso = "^lasso_split_.*\\.rds$",
        RF = "^RF_split_.*\\.rds$",
        XGB = "^XGB_split_.*\\.rds$",
        sparse_log_contrast = "^slc_split_.*\\.rds$"
    )

    map(models, function(pattern) {
        files <- list.files(dir_res, pattern = pattern, full.names = TRUE)
        if (length(files) == 0) {
            tibble()
        } else {
            rows <- map(files, function(f) {
                obj <- readRDS(f)
                tibble(
                    split_seed = as.integer(obj$split_seed),
                    aug_strategy = if (is.null(obj$aug_strategy)) {
                        NA_character_
                    } else {
                        as.character(obj$aug_strategy)
                    },
                    aug_factor = if (is.null(obj$aug_factor)) {
                        NA_integer_
                    } else {
                        as.integer(obj$aug_factor)
                    },
                    model = as.character(obj$model),
                    train_idx = list(obj$train_idx),
                    test_idx = list(obj$test_idx),
                    perf_metrics = list(obj$perf_metrics),
                    roc_curve = list(obj$roc_curve),
                    # Specific for sparse log contrast and logistic regression:
                    lambda_1se = as.numeric(obj$lambda_1se),
                    beta_1se = list(obj$beta_1se),
                    # Specific for RF & XGB: permutation variable importance
                    vimp = list(obj$vimp)
                )
            })
            bind_rows(rows)
        }
    })
}

# Function that creates the boxplot of selected metric for a given model
boxplot_metric <- function(data, add_data = NULL, plot_metric) {
    if (!is.null(add_data)) {
        add_data <- add_data |>
            mutate(aug_strategy = "none_slc", model = "l1_logistic_reg")
    }

    data_plot <- data |>
        bind_rows(add_data) |>
        unnest(perf_metrics) |>
        mutate(
            aug_strategy = case_when(
                is.na(aug_strategy) ~ "Benchmark",
                aug_strategy == "aug_in_p" ~ "randomILR - Aug. in p",
                aug_strategy == "aitchison_mixup" ~ "Aitchison Mixup",
                aug_strategy ==
                    "aitchison_aug_in_p" ~ "Aitchison Mixup & Aug. in p",
                aug_strategy == "none_slc" ~ "Benchmark - Sparse Log Contrast",
                TRUE ~ aug_strategy
            ),
            aug_strategy = factor(
                aug_strategy,
                levels = c(
                    "Benchmark",
                    "Benchmark - Sparse Log Contrast",
                    "Aitchison Mixup",
                    "randomILR - Aug. in p",
                    "Aitchison Mixup & Aug. in p"
                )
            ),
            model = case_when(
                model ==
                    "l1_logistic_reg" ~ "Logistic Regression with L1 Penalty",
                model == "random_forest" ~ "Random Forest",
                model == "xgboost" ~ "XGBoost",
                TRUE ~ model
            )
        )

    if (plot_metric != "misclassification_rate") {
        data_plot <- data_plot |>
            filter(.metric == plot_metric) |>
            mutate(
                .metric = if_else(
                    .metric == "roc_auc",
                    "ROC AUC",
                    .metric |> str_replace("_", " ") |> str_to_title()
                )
            )
    } else {
        data_plot <- data_plot |>
            filter(.metric == "accuracy") |>
            mutate(
                misclas = 1 - .estimate,
                .metric = "Misclassification Rate"
            ) |>
            select(-c(".estimate")) |>
            rename(.estimate = misclas)
    }
    plot_metric_name <- unique(data_plot$.metric)
    model_name <- unique(data_plot$model)
    aug_factor_label <- data_plot |>
        filter(!is.na(aug_factor)) |>
        distinct(aug_factor) |>
        pull(aug_factor)

    data_plot |>
        ggplot(aes(x = aug_strategy, y = .estimate)) +
        # geom_violin(fill = viridis(1), aes()) +
        # geom_boxplot() +
        geom_violin(fill = "steelblue", alpha = 0.5, color = NA) +
        geom_boxplot(
            width = 0.15,
            fill = "steelblue",
            alpha = 0.7,
            outlier.size = 0.8
        ) +
        theme_bw() +
        labs(
            title = paste("Boxplot of", plot_metric_name, "for", model_name),
            subtitle = paste("Augmentation Factor:", aug_factor_label),
            x = "",
            y = plot_metric_name
        ) +
        theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 12)
        ) +
        ylim(0, 1)
}

# Plot of ROC Curve, oneplot per augmentation strategy/ benchmark
roc_curve_plot <- function(data, add_data = NULL) {
    if (!is.null(add_data)) {
        add_data <- add_data |>
            mutate(aug_strategy = "none_slc", model = "l1_logistic_reg")
    }
    # Common FPR grid for averaging curves within each augmentation strategy
    fpr_grid <- seq(0, 1, length.out = 201)

    data_fmt <- data |>
        bind_rows(add_data) |>
        mutate(
            aug_strategy = case_when(
                is.na(aug_strategy) ~ "Benchmark",
                aug_strategy == "aug_in_p" ~ "randomILR - Aug. in p",
                aug_strategy == "aitchison_mixup" ~ "Aitchison Mixup",
                aug_strategy == "none_slc" ~ "Benchmark - Sparse Log Contrast",
                aug_strategy ==
                    "aitchison_aug_in_p" ~ "Aitchison Mixup & Aug. in p",
                TRUE ~ aug_strategy
            ),
            aug_strategy = factor(
                aug_strategy,
                levels = c(
                    "Benchmark",
                    "Benchmark - Sparse Log Contrast",
                    "Aitchison Mixup",
                    "randomILR - Aug. in p",
                    "Aitchison Mixup & Aug. in p"
                )
            ),
            model = case_when(
                model ==
                    "l1_logistic_reg" ~ "Logistic Regression with L1 Penalty",
                model == "random_forest" ~ "Random Forest",
                model == "xgboost" ~ "XGBoost",
                model ==
                    "sparse_log_contrast" ~ "Sparse Log Contrast Regression",
                TRUE ~ model
            )
        )

    data_plot <- data_fmt |>
        unnest(roc_curve) |>
        mutate(fpr = 1 - specificity) |>
        arrange(aug_strategy, split_seed, fpr)

    # data_mean <- data_plot |>
    #     group_by(aug_strategy, fpr) |>
    #     summarise(
    #         y_mean = mean(sensitivity, na.rm = TRUE),
    #         y_sd = sd(sensitivity, na.rm = TRUE),
    #         .groups = "drop"
    #     ) |>
    #     mutate(
    #         y_sd = tidyr::replace_na(y_sd, 0),
    #         err_low = pmax(0, y_mean - y_sd),
    #         err_high = pmin(1, y_mean + y_sd)
    #     )

    # Interpolate each split’s ROC onto a common FPR grid, then average
    data_mean <- data_plot |>
        group_by(aug_strategy, split_seed) |>
        group_modify(
            ~ {
                df <- arrange(.x, fpr)
                tibble(
                    fpr = fpr_grid,
                    sensitivity = approx(
                        x = df$fpr,
                        y = df$sensitivity,
                        xout = fpr_grid,
                        ties = "ordered",
                        rule = 2
                    )$y
                )
            }
        ) |>
        group_by(aug_strategy, fpr) |>
        summarise(
            y_mean = mean(sensitivity, na.rm = TRUE),
            y_sd = sd(sensitivity, na.rm = TRUE),
            .groups = "drop"
        ) |>
        mutate(
            y_sd = tidyr::replace_na(y_sd, 0),
            err_low = pmax(0, y_mean - y_sd),
            err_high = pmin(1, y_mean + y_sd)
        )

    mean_roc_auc <- data_fmt |>
        unnest(perf_metrics) |>
        filter(.metric == "roc_auc") |>
        group_by(aug_strategy) |>
        summarise(mean_auc = mean(.estimate, na.rm = TRUE), .groups = "drop")

    lab_df <- mean_roc_auc |>
        mutate(
            x = -Inf,
            y = Inf,
            label = paste("Mean AUC", round(mean_auc, 2))
        )

    model_name <- unique(data_plot$model)

    ggplot(
        data_plot,
        aes(
            x = fpr,
            y = sensitivity,
            group = interaction(split_seed, aug_strategy)
        )
    ) +
        geom_ribbon(
            data = data_mean,
            aes(
                x = fpr,
                ymin = err_low,
                ymax = err_high
            ),
            fill = "grey70",
            inherit.aes = FALSE,
            alpha = 0.5
        ) +
        geom_line(
            linetype = 3,
            alpha = 0.5,
            colour = "grey40",
            linewidth = 0.7
        ) +
        geom_line(
            data = data_mean,
            aes(
                x = fpr,
                y = y_mean
            ),
            color = "#C24841",
            inherit.aes = FALSE,
            linewidth = 1
        ) +
        facet_wrap(~aug_strategy, ncol = 3) +
        geom_abline(
            slope = 1,
            intercept = 0,
            linetype = 2,
            colour = "steelblue",
            linewidth = 1
        ) +
        labs(
            x = "False Positive Rate",
            y = "True Positive Rate",
            title = paste(
                model_name,
                "ROC Curves by Augmentation Strategy"
            ),
            subtitle = "Grey: ROC per split, red: mean curve, band: mean \u00b1 1 sd"
        ) +
        geom_text(
            data = lab_df,
            mapping = aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            hjust = -0.1,
            vjust = 1.3,
            size = 4.5
        ) +
        coord_cartesian(clip = "off", xlim = c(0, 1), ylim = c(0, 1)) +
        theme_bw() +
        theme(
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 12)
        )
}

# Function to create summary table
summary_tbl_mean_perf <- function(data, add_data = NULL) {
    if (!is.null(add_data)) {
        add_data <- add_data |>
            mutate(aug_strategy = "none_slc", model = "l1_logistic_reg")
    }

    data_tbl <- data |>
        bind_rows(add_data) |>
        mutate(
            aug_strategy = case_when(
                is.na(aug_strategy) ~ "Benchmark",
                aug_strategy == "aug_in_p" ~ "randomILR - Aug. in p",
                aug_strategy == "aitchison_mixup" ~ "Aitchison Mixup",
                aug_strategy ==
                    "aitchison_aug_in_p" ~ "Aitchison Mixup & Aug. in p",
                aug_strategy == "none_slc" ~ "Benchmark - Sparse Log Contrast",
                TRUE ~ aug_strategy
            ),
            aug_strategy = factor(
                aug_strategy,
                levels = c(
                    "Benchmark",
                    "Benchmark - Sparse Log Contrast",
                    "Aitchison Mixup",
                    "randomILR - Aug. in p",
                    "Aitchison Mixup & Aug. in p"
                )
            ),
            model = case_when(
                model ==
                    "l1_logistic_reg" ~ "Logistic Regression with L1 Penalty",
                model == "random_forest" ~ "Random Forest",
                model == "xgboost" ~ "XGBoost",
                TRUE ~ model
            )
        )

    data_misclass <- data_tbl |>
        unnest(perf_metrics) |>
        filter(.metric == "accuracy") |>
        mutate(
            misclas = 1 - .estimate,
            .metric = "Misclassification Rate"
        ) |>
        select(-c(".estimate")) |>
        rename(.estimate = misclas)

    data_summary <- data_tbl |>
        unnest(perf_metrics) |>
        bind_rows(data_misclass) |>
        group_by(aug_strategy, .metric) |>
        summarise(mean_metric = mean(.estimate)) |>
        mutate(
            mean_metric = round(mean_metric, 2),
            Metric = case_when(
                .metric == "accuracy" ~ "Accuracy",
                .metric == "roc_auc" ~ "ROC AUC",
                .metric == "brier_class" ~ "Brier class",
                TRUE ~ str_to_title(.metric)
            ),
            Metric = factor(
                Metric,
                levels = c(
                    "Accuracy",
                    "Misclassification Rate",
                    "ROC AUC",
                    "Brier class"
                )
            )
        ) |>
        pivot_wider(
            names_from = aug_strategy,
            values_from = mean_metric
        ) |>
        mutate(
            across(-.metric, ~ replace_na(as.character(.x), "-"))
        ) |>
        select(-.metric)

    knitr::kable(
        data_summary,
        captation = "Mean perfromance metrics across augmentation strategies"
    )
}

boxplot_density <- function(data, plot_metric) {
    data_plot <- data |>
        filter(
            aug_strategy != "aitchison_mixup",
            !is.na(aug_strategy)
        ) |>
        mutate(
            density = str_extract(
                aug_strategy,
                "(?<=dens_)(?:-?(?:\\d*\\.\\d+|\\d+)|NA)"
            ),
            density = if_else(density == "NA", "Unit", density)
        ) |>
        unnest(perf_metrics)

    if (plot_metric != "misclassification_rate") {
        data_plot <- data_plot |>
            filter(.metric == plot_metric) |>
            mutate(
                .metric = if_else(
                    .metric == "roc_auc",
                    "ROC AUC",
                    .metric |> str_replace("_", " ") |> str_to_title()
                )
            )
    } else {
        data_plot <- data_plot |>
            filter(.metric == "accuracy") |>
            mutate(
                misclas = 1 - .estimate,
                .metric = "Misclassification Rate"
            ) |>
            select(-c(".estimate")) |>
            rename(.estimate = misclas)
    }

    data_plot <- data_plot |>
        mutate(
            aug_strategy = str_remove_all(
                aug_strategy,
                "_dens_(?:NA|\\d+\\.\\d{1,2})"
            ),
            aug_strategy = case_when(
                aug_strategy == "aug_in_p" ~ "randomILR - Aug. in p",
                aug_strategy ==
                    "aitchison_aug_in_p" ~ "Aitchison Mixup & Aug. in p",
                TRUE ~ aug_strategy
            ),
            model = case_when(
                model ==
                    "l1_logistic_reg" ~ "Logistic Regression with L1 Penalty",
                model == "random_forest" ~ "Random Forest",
                model == "xgboost" ~ "XGBoost",
                TRUE ~ model
            ),
            density = factor(
                density,
                levels = c(
                    "Unit",
                    seq(0.05, 0.5, by = 0.05) |> as.character()
                )
            )
        )

    plot_metric_name <- unique(data_plot$.metric) |> as.character()
    model_name <- unique(data_plot$model)
    aug_factor_label <- unique(data_plot$aug_factor)

    y_limits <- switch(
        plot_metric_name,
        "ROC AUC" = c(0.5, 1),
        "Misclassification Rate" = c(0, 0.5),
        "Accuracy" = c(0.5, 1),
        "Brier Class" = c(0, 0.5),
        NULL
    )

    data_plot |>
        ggplot(aes(x = density, y = .estimate)) +
        geom_violin(fill = "steelblue", alpha = 0.5, color = NA) +
        geom_boxplot(
            width = 0.15,
            fill = "steelblue",
            alpha = 0.7,
            outlier.size = 0.8
        ) +
        theme_bw() +
        facet_wrap(~aug_strategy) +
        labs(
            title = paste(
                "Impact of density on",
                plot_metric_name,
                "for",
                model_name
            ),
            subtitle = paste("Augmentation Factor:", aug_factor_label),
            x = "Density",
            y = plot_metric_name
        ) +
        theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 12)
        ) +
        (if (is.null(y_limits)) NULL else coord_cartesian(ylim = y_limits))
}

# Table summarising total number of beta coefficients and average
# number of non-zero beta coefs (average across all 20 test-train splits)
log_reg_summary_tbl <- function(res) {
    res$lasso |>
        select(split_seed, aug_strategy, aug_factor, beta_1se) |>
        unnest(beta_1se) |>
        group_by(aug_strategy, split_seed) |>
        summarise(
            n_nonzero = sum(estimate != 0),
            total = n(),
            .groups = "drop"
        ) |>
        group_by(aug_strategy) |>
        summarise(
            mean_nonzero = mean(n_nonzero) |> round(),
            total = first(total),
            .groups = "drop"
        ) |>
        bind_rows(
            res$sparse_log_contrast |>
                select(split_seed, aug_strategy, aug_factor, beta_1se) |>
                unnest(beta_1se) |>
                group_by(aug_strategy, split_seed) |>
                summarise(
                    n_nonzero = sum(estimate != 0),
                    total = n(),
                    .groups = "drop"
                ) |>
                group_by(aug_strategy) |>
                summarise(
                    mean_nonzero = mean(n_nonzero) |> round(),
                    total = first(total),
                    .groups = "drop"
                ) |>
                mutate(aug_strategy = "none_slc")
        ) |>
        mutate(
            aug_strategy = case_when(
                is.na(aug_strategy) ~ "Benchmark",
                aug_strategy == "aug_in_p" ~ "randomILR - Aug. in p",
                aug_strategy == "aitchison_mixup" ~ "Aitchison Mixup",
                aug_strategy ==
                    "aitchison_aug_in_p" ~ "Aitchison Mixup & Aug. in p",
                aug_strategy == "none_slc" ~ "Benchmark - Sparse Log Contrast",
                TRUE ~ aug_strategy
            ),
            aug_strategy = factor(
                aug_strategy,
                levels = c(
                    "Benchmark",
                    "Benchmark - Sparse Log Contrast",
                    "Aitchison Mixup",
                    "randomILR - Aug. in p",
                    "Aitchison Mixup & Aug. in p"
                )
            )
        ) |>
        arrange(aug_strategy) |>
        knitr::kable(
            col.names = c(
                "Data Variant",
                "Average # of non‑zero $\\beta$’s",
                "Total # of $\\beta$’s"
            ),
            escape = FALSE,
            caption = "Summary of estimated coefficients"
        )
}

# On avg, the number of standard vs random ilr coordinates selected in:
#   aug in p
#   Aitchison + aug in p
avg_random_vs_standard <- function(res) {
    selected_ilr <- res$lasso |>
        filter(aug_strategy %in% c("aug_in_p", "aitchison_aug_in_p")) |>
        select(-c(train_idx:roc_curve)) |>
        unnest(beta_1se) |>
        filter(estimate != 0, term != "(Intercept)") |>
        mutate(
            idx = str_remove(term, "V"),
            part = if_else(
                str_detect(idx, "_\\d+$"),
                "random_ilr",
                "standard_ilr"
            ),
            rand_ilr_idx = if_else(
                str_detect(idx, "_\\d+$"),
                as.numeric(str_extract(idx, "(?<=_)\\d+$")),
                NA_real_
            ),
            idx = str_remove(idx, "_\\d+$") |> as.numeric()
        )

    # Average number of selected coordinates by part across split seeds
    avg_part_counts <- selected_ilr |>
        count(aug_strategy, split_seed, part, name = "n_rows") |>
        group_by(aug_strategy, part) |>
        summarise(
            avg_rows = mean(n_rows, na.rm = TRUE) |> round(),
            .groups = "drop"
        ) |>
        tidyr::pivot_wider(
            names_from = part,
            values_from = avg_rows,
            names_prefix = "avg_"
        ) |>
        mutate(
            aug_strategy = case_when(
                aug_strategy == "aug_in_p" ~ "randomILR - Aug. in p",
                aug_strategy ==
                    "aitchison_aug_in_p" ~ "Aitchison Mixup & Aug. in p",
                TRUE ~ aug_strategy
            ),
            aug_strategy = factor(
                aug_strategy,
                levels = c(
                    "randomILR - Aug. in p",
                    "Aitchison Mixup & Aug. in p"
                )
            )
        ) |>
        arrange(aug_strategy) |>
        select(aug_strategy, avg_standard_ilr, avg_random_ilr)

    knitr::kable(
        avg_part_counts,
        col.names = c(
            "Data Variant",
            "Average # of non‑zero $\\beta$’s - standard ILR coordinate",
            "Average # of non‑zero $\\beta$’s - random ILR coordinate"
        ),
        escape = FALSE,
        caption = "Non-zero $\\beta$ coefficients: standard vs. random ILR coordinates"
    )
}

# For a selected ilr coordinates (with beta coefs != 0 or VIMP in top k)
# corres. to standard ilr makes a summary table of original features that
# have positive vs. negative sign in the basis matrix
summarise_signs <- function(mat, sel_idx = seq_len(ncol(mat))) {
    stopifnot(is.matrix(mat), length(sel_idx) == ncol(mat))

    seq_len(ncol(mat)) |>
        purrr::map(function(j) {
            pos <- which(mat[, j] > 0)
            neg <- which(mat[, j] < 0)

            tibble::tibble(
                ilr_coordinate = sel_idx[j],
                positive = if (length(pos) == 0) {
                    NA_character_
                } else {
                    paste(pos, collapse = ", ")
                },
                negative = if (length(neg) == 0) {
                    NA_character_
                } else if (length(neg) == 1) {
                    as.character(neg)
                } else {
                    paste0(min(neg), ":", max(neg))
                }
            )
        }) |>
        dplyr::bind_rows()
}

# Top k contributors - top k original features that contribute most to
# the selected random ilr ilr coordinates (lasso: coords with beta != 0, RF & XGB: ilr coordinates with the VIMP in top k)
# Return: list, each element of the list is a tibble
# Tibble with columns:
# row_idx col has the idx of original features that are top k contributors
# value is the entry from GHL matrix that is associated with that feature for a given ILR coordinate
# sign - is value negative or positive number?

top_k_row_indices <- function(mat, top_k = 5, sel_idx = NULL) {
    stopifnot(is.matrix(mat), top_k >= 1)
    if (is.null(sel_idx)) {
        sel_idx <- seq_len(ncol(mat))
    }

    abs_mat <- abs(mat)

    out <- lapply(seq_len(ncol(abs_mat)), function(j) {
        idx <- order(abs_mat[, j], decreasing = TRUE, na.last = NA)
        idx <- head(idx, min(top_k, length(idx)))
        tibble::tibble(
            row_idx = idx,
            value = mat[idx, j],
            sign = dplyr::case_when(
                value > 0 ~ "positive",
                value < 0 ~ "negative",
                TRUE ~ "zero"
            )
        )
    })

    names(out) <- paste0("ilr_part_", sel_idx)
    out
}

# Summarise contributors (original features among top k contributors to the ilr coordinates)
# for the datasets augmented in p
contributors_aug_p <- function(
    res,
    sel_split,
    sel_aug_strategy,
    top_contr = 3, # number of original features most contributing to num_top_feat ILR coordinates
    num_top_feat = 7, # number of ilr coordinates with the highest VIMP that should be analysed
    num_coefs = NULL, # for lasso: limit to top |beta| coefficients
    ilr_mat, # needs to correspond to selected seed!
    model #one of "lasso", "RF", "XGB"
) {
    # Select results for a given seed & aug strategy
    model_res <- res[[model]] |>
        filter(aug_strategy == sel_aug_strategy, split_seed == sel_split)

    # Character needed to extract ilr matricies for the selected aug strategy
    ilr_mat_name <- if (sel_aug_strategy == "aug_in_p") {
        "aug_p"
    } else if (sel_aug_strategy == "aitchison_aug_in_p") {
        "aitchison_aug_p"
    }

    # For 1 split:
    # This vector gives me the indicies of the ilr coordinates that are important
    if (model == "lasso") {
        # in lasso case meaning they have non-zero beta coefficients
        idx_coord_sel_aug <- model_res |>
            select(-c(train_idx:roc_curve)) |>
            unnest(beta_1se) |>
            filter(estimate != 0, term != "(Intercept)") |>
            mutate(abs_estm = abs(estimate))

        if (!is.null(num_coefs)) {
            idx_coord_sel_aug <- idx_coord_sel_aug |>
                slice_max(order_by = abs_estm, n = num_coefs, with_ties = FALSE)
        }

        idx_coord_sel_aug <- idx_coord_sel_aug |>
            mutate(
                idx = str_remove(term, "V"),
                part = if_else(
                    str_detect(idx, "_\\d+$"),
                    "random_ilr",
                    "standard_ilr"
                ),
                rand_ilr_idx = if_else(
                    str_detect(idx, "_\\d+$"),
                    as.numeric(str_extract(idx, "(?<=_)\\d+$")),
                    NA_real_
                ),
                idx = str_remove(idx, "_\\d+$") |> as.numeric()
            ) |>
            select(-split_seed, -aug_strategy, -abs_estm)
    } else {
        idx_coord_sel_aug <- model_res |>
            select(-c(train_idx:beta_1se)) |>
            unnest(vimp) |>
            arrange(desc(importance)) |>
            mutate(
                idx = str_remove(predictor, "V"),
                part = if_else(
                    str_detect(idx, "_\\d+$"),
                    "random_ilr",
                    "standard_ilr"
                ),
                rand_ilr_idx = if_else(
                    str_detect(idx, "_\\d+$"),
                    as.numeric(str_extract(idx, "(?<=_)\\d+$")),
                    NA_real_
                ),
                idx = str_remove(idx, "_\\d+$") |> as.numeric()
            ) |>
            # to make sure that I only consider positive importance:
            filter(importance > 0) |>
            slice_head(n = num_top_feat)
    }

    parts <- unique(idx_coord_sel_aug$part)
    standard_ilr_tbl <- tibble::tibble(
        ilr_coordinate = numeric(),
        positive = character(),
        negative = character()
    )
    top_k_rand_ilr_list <- list()

    for (p in parts) {
        temp <- idx_coord_sel_aug |> filter(part == p)
        if (p == "standard_ilr") {
            # Extracting ilr matrix that corresponds to standard ilr part
            col_idx <- temp$idx
            base_mat <- ilr_mat[[ilr_mat_name]]$standard_ilr[,
                col_idx,
                drop = FALSE
            ] |>
                round(3)
            # Summarise contribution of original features
            standard_ilr_tbl <- summarise_signs(base_mat, col_idx)
        } else {
            unique_rand_ilr <- unique(temp$rand_ilr_idx)
            for (idx_mat in unique_rand_ilr) {
                col_idx <- temp |> filter(rand_ilr_idx == idx_mat) |> pull(idx)
                # Extracting ilr matrix that corresponds to random ilr part
                base_mat <- ilr_mat[[ilr_mat_name]]$random_ilr[[idx_mat]][,
                    col_idx
                ] |>
                    round(3) |>
                    as.matrix()
                top_k <- top_k_row_indices(
                    base_mat,
                    top_k = top_contr,
                    sel_idx = col_idx
                )
                entry_name <- paste0("rand_ilr_", idx_mat)
                top_k_rand_ilr_list[[entry_name]] <- tibble::tibble(
                    segment = entry_name,
                    ilr_coordinate = names(top_k),
                    contributors = top_k
                )
            }
        }
    }

    top_k_rand_ilr_tbl <- if (length(top_k_rand_ilr_list) == 0) {
        tibble::tibble(
            segment = character(),
            ilr_coordinate = character(),
            row_idx = integer(),
            value = numeric(),
            sign = character()
        )
    } else {
        top_k_rand_ilr_list |>
            dplyr::bind_rows() |>
            tidyr::unnest(contributors)
    }

    list(standard_ilr = standard_ilr_tbl, random_ilr = top_k_rand_ilr_tbl)
}


contributors_all_splits <- function(
    res,
    dir_results,
    sel_aug_strategy,
    num_top_feat = 7,
    top_contr = 3,
    num_coefs = NULL,
    model #one of "lasso", "RF", "XGB"
) {
    aug_p_contr_tbl <- map(1:20, function(one_split) {
        name <- sprintf("ilr_basis_split_%s.rds", one_split)
        ilr_one_split <- readRDS(file.path(dir_results, name))
        res_one <- contributors_aug_p(
            res = res,
            sel_split = one_split,
            sel_aug_strategy = sel_aug_strategy,
            ilr_mat = ilr_one_split,
            top_contr = top_contr,
            num_top_feat = num_top_feat,
            num_coefs = num_coefs,
            model = model
        )
        tibble(
            split = one_split,
            standard_ilr = list(res_one$standard_ilr),
            random_ilr = list(res_one$random_ilr)
        )
    }) |>
        bind_rows()

    aug_p_contr_tbl
}


# Heatmap showing the sign of top k contibuting features to the ilr coordinates that had beta != 0
heatmap_contr <- function(contr_tbl, sel_split, cov_names) {
    aug_p_one_split <- contr_tbl |>
        filter(split == sel_split) |>
        unnest(random_ilr) |>
        mutate(
            ilr_coordinate = factor(ilr_coordinate),
            segment = if_else(segment == "rand_ilr_1", "GHL 1", "GHL 2"),
            ilr_coordinate = str_replace(
                ilr_coordinate,
                "ilr_part_",
                "Coordinate "
            ),
            sign = if_else(sign == "positive", "Numerator", "Denominator")
        )

    aug_p_one_split |>
        left_join(cov_names, by = "row_idx") |>
        mutate(
            covariate = factor(covariate, levels = sort(unique(covariate)))
        ) |>
        ggplot(aes(x = covariate, y = ilr_coordinate, fill = sign)) +
        geom_tile(color = "white") +
        scale_fill_manual(
            values = c(Numerator = "#d7191c", Denominator = "#2b83ba")
        ) +
        facet_wrap(~segment, scales = "free_y", ncol = 1) +
        labs(
            x = "Original feature",
            y = "Selected ILR coordinate",
            fill = "Contribution",
            title = "Top 3 contributors to ILR coordinates selected by a model",
            subtitle = paste("Data split:", sel_split)
        ) +
        theme_bw() +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom"
        )
}
# Summary of contributors form standard ilr part for 1 data split
# This function takes as the input tibble with the structure as produced by contributors_all_splits()
standard_ilr_one_split <- function(contr_tbl, sel_split) {
    contr_tbl |>
        filter(split == sel_split) |>
        unnest(standard_ilr) |>
        select(-split, -any_of("random_ilr")) |>
        rename(
            `ILR coordinate` = ilr_coordinate,
            `Numerator features` = positive,
            `Denominator features` = negative
        ) |>
        knitr::kable(
            caption = paste(
                "Selected ILR coordinates (standard ILR transformation) and their contributors - data split",
                sel_split
            )
        )
}

# Get contributors from models trained on only standard ilr transformed data
# appropiate for reults of benchmark or Aitchison augmented models
contributors_stand_ilr_one_split <- function(
    res,
    sel_split,
    sel_aug_strategy,
    model, #one of RF, XGB, lasso
    ilr_mat, # needs to correspond to selected seed!
    num_top_feat = 3, #for RF and XGB: how many features with the highest vimp should be selected
    num_coefs = NULL # number of non-zero beta coaffs that should be analysed
) {
    # Select results for a given seed & aug strategy
    if (!is.na(sel_aug_strategy)) {
        res_model <- res[[model]] |>
            filter(aug_strategy == sel_aug_strategy, split_seed == sel_split)
    } else {
        res_model <- res[[model]] |>
            filter(is.na(aug_strategy), split_seed == sel_split)
    }

    # For 1 split:
    # This vector gives me the indicies of the ilr coordinates that are important
    if (model == "lasso") {
        # so in the case of lasso it means that beta coef != 0
        idx_coord_sel_aug <- res_model |>
            select(-c(train_idx:roc_curve)) |>
            unnest(beta_1se) |>
            filter(estimate != 0, term != "(Intercept)") |>
            mutate(abs_estm = abs(estimate))

        if (!is.null(num_coefs)) {
            idx_coord_sel_aug <- idx_coord_sel_aug |>
                slice_max(order_by = abs_estm, n = num_coefs, with_ties = FALSE)
        }

        idx_coord_sel_aug <- idx_coord_sel_aug |>
            mutate(idx = str_remove(term, "V") |> as.numeric()) |>
            select(-split_seed, -aug_strategy, -abs_estm)
    } else {
        # For all other models, it means that ilr coordinate has top k vimp
        idx_coord_sel_aug <- res_model |>
            select(-c(train_idx:beta_1se)) |>
            unnest(vimp) |>
            arrange(desc(importance)) |>
            mutate(
                idx = str_remove(predictor, "V") |> as.numeric()
            ) |>
            # for RF & XGB - select top predictors
            slice_head(n = num_top_feat)
    }

    # Extracting ilr matrix that corresponds to standard ilr part
    col_idx <- idx_coord_sel_aug$idx
    base_mat <- ilr_mat$standard_ilr[, col_idx] |>
        round(3)
    # Summarise contribution of original features
    summarise_signs(base_mat, col_idx)
}

# Wrapper function to map contributors to selected STANDARD ilr coordinates
# across 20 test-train data splits
contr_standard_ilr_all_splits <- function(
    res,
    dir_results,
    sel_aug_strategy,
    model, #one of "lasso", "XGB", "RF"
    num_top_feat = 3, # number of features with the highest importance to analyse (needed for XGB and RF)
    num_coefs = NULL
) {
    contr_tbl <- map(1:20, function(one_split) {
        name <- sprintf("ilr_basis_split_%s.rds", one_split)
        ilr_one_split <- readRDS(file.path(dir_results, name))
        res_one <- contributors_stand_ilr_one_split(
            res = res,
            sel_split = one_split,
            sel_aug_strategy = sel_aug_strategy,
            model = model,
            ilr_mat = ilr_one_split,
            num_top_feat = num_top_feat,
            num_coefs = num_coefs
        )
        tibble(
            split = one_split,
            standard_ilr = list(res_one)
        )
    }) |>
        bind_rows()

    contr_tbl
}


# Helper to summarise and display the most frequent numerator contributors in the standard ILR coordinates
top_k_standard_ilr <- function(tbls, top_n, section_titles, cov_names) {
    stopifnot(
        length(tbls) == length(top_n),
        length(section_titles) == length(tbls),
        all(c("row_idx", "covariate") %in% names(cov_names))
    )

    prep_tbl <- function(tbl, n_keep) {
        tbl |>
            unnest(standard_ilr) |>
            count(positive) |>
            mutate(row_idx = as.integer(positive)) |>
            left_join(cov_names, by = "row_idx") |>
            transmute(
                feature = if_else(
                    !is.na(covariate),
                    covariate,
                    as.character(row_idx)
                ),
                row_idx,
                n
            ) |>
            arrange(desc(n), feature) |>
            head(n_keep)
    }

    tbl_list <- map2(tbls, top_n, prep_tbl)
    row_counts <- map_int(tbl_list, nrow)
    section_info <- tibble(
        title = section_titles,
        rows = row_counts
    ) |>
        mutate(
            end = cumsum(rows),
            start = end - rows + 1
        ) |>
        filter(rows > 0)

    combined_tbl <- bind_rows(tbl_list) |>
        knitr::kable(
            col.names = c(
                "Original feature in numerator",
                "Feature column idx",
                "# of appearances"
            ),
            caption = "Features contibuting in numerator most often to the selected standard ILR coordinates",
            align = c("l", "c", "c"),
            escape = FALSE
        ) |>
        kableExtra::kable_styling(full_width = TRUE)

    reduce(
        seq_len(nrow(section_info)),
        .init = combined_tbl,
        .f = ~ kableExtra::group_rows(
            .x,
            section_info$title[.y],
            section_info$start[.y],
            section_info$end[.y]
        )
    )
}

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
                    beta_1se = list(obj$beta_1se)
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
            )
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
            density = factor(density, levels = c(NA, seq(0.05, 0.5, by = 0.05)))
        )

    plot_metric_name <- unique(data_plot$.metric)
    model_name <- unique(data_plot$model)
    aug_factor_label <- unique(data_plot$aug_factor)

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

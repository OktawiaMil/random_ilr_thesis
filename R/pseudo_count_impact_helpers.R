# Function to prepare the dataset such that later one can compute
# the differences in th selected performance metric between results
# for the model trained on the data with PC= A and PC = B
prep_input_data <- function(
    res_pc,
    aug_factor = 2,
    plot_metric = c(
        "roc_auc",
        "accuracy",
        "misclassification_rate"
    )
) {
    plot_metric <- match.arg(plot_metric)

    # Check if input data is the random ILR results
    col_names <- colnames(res_pc)
    if ("density" %in% col_names) {
        # res_pc is random ILR result - add density of skew-symm. matrix
        # to the augmentation column
        res_pc <- res_pc |>
            mutate(augmentation_den = paste0(augmentation, "_", density)) |>
            select(-c(augmentation, density)) |>
            rename(augmentation = augmentation_den)
    } else if ("transform" %in% col_names) {
        # Input res_pc is benchmark model
        res_pc <- res_pc |>
            mutate(model_trans = paste0("Benchmark_", transform)) |>
            select(-"transform") |>
            rename(augmentation = model_trans) |>
            mutate(augmentation_factor = aug_factor)
    }

    # Filter for desired augmentation factor & perfromance metric
    data <- res_pc |>
        filter(augmentation_factor == aug_factor) |>
        unnest(perf_metrics)

    if (plot_metric != "misclassification_rate") {
        data <- data |>
            filter(.metric == plot_metric) |>
            mutate(
                .metric = if_else(
                    .metric == "roc_auc",
                    "ROC AUC",
                    .metric |> str_replace("_", " ") |> str_to_title()
                )
            )
    } else {
        data <- data |>
            filter(.metric == "accuracy") |>
            mutate(
                misclas = 1 - .estimate,
                .metric = "Misclassification Rate"
            ) |>
            select(-c(".estimate")) |>
            rename(.estimate = misclas)
    }
}

# Compute the difference in the selected performance metric between 2 tibbles
# Difference: res_pc1- res_pc2
diff_in_metric <- function(res_pc1, res_pc2) {
    keys <- c(
        "data_id",
        "split",
        "model",
        "augmentation",
        "augmentation_factor",
        ".metric"
    )

    combined <- bind_rows(
        res_pc1 %>%
            mutate(source = "res_pc1"),
        res_pc2 %>%
            mutate(source = "res_pc2")
    )

    pc_levels <- combined %>%
        distinct(source, pseudo_count) %>%
        arrange(source) %>%
        pull(pseudo_count)

    if (length(pc_levels) != 2) {
        abort(glue::glue(
            "Expected exactly two pseudo_count values, got: {toString(pc_levels)}"
        ))
    }

    wide <- combined %>%
        select(all_of(keys), pseudo_count, .estimate) %>%
        pivot_wider(
            names_from = pseudo_count,
            values_from = .estimate,
            names_glue = "estimate_{pseudo_count}"
        )

    col_a <- paste0("estimate_", pc_levels[1])
    col_b <- paste0("estimate_", pc_levels[2])

    wide %>%
        mutate(diff_perf = .data[[col_a]] - .data[[col_b]]) %>%
        relocate(diff_perf)
}


# Compute mean difference in the selected performance metric between 2 models
# that differ only by the pseudo-count used in the input data
# Difference: res_pc1 - res_pc2
mean_diff_perf <- function(
    res_pc1,
    res_pc2,
    aug_factor,
    plot_metric = c(
        "roc_auc",
        "accuracy",
        "misclassification_rate"
    )
) {
    # Prepare input data - filter for selected aug_factor and performance metric
    data1 <- prep_input_data(
        res_pc = res_pc1,
        aug_factor = aug_factor,
        plot_metric = plot_metric
    )
    data2 <- prep_input_data(
        res_pc = res_pc2,
        aug_factor = aug_factor,
        plot_metric = plot_metric
    )
    # Differences per data_id, split, model, augmentation, augmentation_factor, metric
    diff <- diff_in_metric(data1, data2)

    # Compute mean difference
    mean_diff <- diff |>
        group_by(data_id, augmentation, augmentation_factor, .metric) |>
        summarise(mean_diff = mean(diff_perf)) |>
        ungroup()

    return(mean_diff)
}


# Create the heatmap showing the mean difference in selected perf. metric
# between models trained on the data with PC = A and PC = B
mean_diff_heatmap <- function(
    rand_ilr_half,
    rand_ilr_max,
    rod_half,
    rod_max,
    bench_half,
    bench_max,
    aug_factor = 2,
    plot_metric = "roc_auc"
) {
    # Get model name and format it (needed for the title on the plot)
    model <- unique(rod_half$model)

    if (model == "lasso") {
        model <- "Logistic Regression with L1 Penalty"
    } else if (model == "lasso_ilr") {
        model <- "Logistic Regression (ILR-trans. Data) with L1 Penalty"
    } else if (model == "xgboost") {
        model <- "XGBoost"
    } else {
        model <- model |> str_replace("_", " ") |> str_to_title()
    }

    # Compute mean differences in per. metric bewteen 2 pseudo-counts
    ## Rodriguez
    mean_rod <- mean_diff_perf(
        rod_half,
        rod_max,
        aug_factor = aug_factor,
        plot_metric = plot_metric
    )

    ## random ILR
    mean_ilr <- mean_diff_perf(
        rand_ilr_half,
        rand_ilr_max,
        aug_factor = aug_factor,
        plot_metric = plot_metric
    )

    ## Benchmark
    mean_bench <- mean_diff_perf(
        bench_half,
        bench_max,
        aug_factor = aug_factor,
        plot_metric = plot_metric
    )

    # Bind into 1 dataset
    data_plot <- bind_rows(
        mean_rod,
        mean_ilr,
        mean_bench
    )

    # Make nice augmentation labels
    data_plot <- data_plot %>%
        mutate(
            augmentation = case_when(
                augmentation == "Benchmark_proportion" ~
                    "Benchmark - Proportion",
                augmentation == "Benchmark_standard_ilr" ~
                    "Benchmark - Standard ILR",
                augmentation == "aitchison_mixup" ~ "Aitchison Mixup",
                augmentation == "comp_cutmix" ~ "Comp. Cutmix",
                augmentation == "comp_feature" ~ "Comp. Feature Dropout",
                augmentation == "aug_in_n_0.1" ~ "RandomILR in n, den. 0.1",
                augmentation == "aug_in_n_0.5" ~ "RandomILR in n, den. 0.5",
                augmentation == "aug_in_n_NA" ~ "RandomILR in n, den. unit",
                augmentation == "aug_in_p_0.1" ~ "RandomILR in p, den. 0.1",
                augmentation == "aug_in_p_0.5" ~ "RandomILR in p, den. 0.5",
                augmentation == "aug_in_p_NA" ~ "RandomILR in p, den. unit",
                TRUE ~ augmentation
            )
        )

    desired_order <- c(
        "Benchmark - Proportion",
        "Benchmark - Standard ILR",
        "Aitchison Mixup",
        "Comp. Cutmix",
        "Comp. Feature Dropout",
        "RandomILR in p, den. unit",
        "RandomILR in p, den. 0.1",
        "RandomILR in p, den. 0.5",
        "RandomILR in n, den. unit",
        "RandomILR in n, den. 0.1",
        "RandomILR in n, den. 0.5"
    )

    data_plot <- data_plot %>%
        mutate(
            data_id = as.numeric(data_id),
            data_id = factor(data_id, levels = sort(unique(data_id))),
            augmentation = factor(augmentation, levels = rev(desired_order)),
            mean_diff = round(mean_diff, 3)
        )

    # Get the limits on the colour scale:
    # bigger number (in abs) out of min and max of observed difference
    limit <- max(abs(range(data_plot$mean_diff, na.rm = TRUE)))
    limit <- round(limit, 3)

    # Scale color depends on the metric(good means something different for 2 metrics)
    # negative misclassification rate is good, whereas for roc auc positive change is good
    if (
        plot_metric %in% c("misclassification_rate", "missclassification_rate")
    ) {
        col_scale <- list(negative_chg = "#2b83ba", positive_chg = "#d7191c")
    } else {
        col_scale <- list(negative_chg = "#d7191c", positive_chg = "#2b83ba")
    }

    metric_name <- unique(data_plot$.metric)
    ggplot(data_plot, aes(x = data_id, y = augmentation, fill = mean_diff)) +
        geom_tile(color = "grey85") +
        scale_fill_gradient2(
            low = col_scale$negative_chg,
            mid = "white",
            high = col_scale$positive_chg,
            midpoint = 0,
            limits = c(-limit, limit),
            name = "Mean difference"
        ) +
        labs(
            title = paste(
                "Mean Difference in",
                metric_name,
                "-",
                model
            ),
            subtitle = paste(
                "Pseudo-count 0.5 vs 1/max library size\nAugmentation Factor:",
                aug_factor
            ),
            x = "Data ID",
            y = "",
            fill = "Mean difference"
        ) +
        theme_bw() +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 12)
        )
}

# Render heatmaps across available augmentation factors,
# printing each plot inside its own tab
render_mean_diff_heatmap_tabs <- function(
    rand_ilr_half,
    rand_ilr_max,
    rod_half,
    rod_max,
    bench_half,
    bench_max,
    plot_metric = "roc_auc",
    aug_factors = NULL,
    heading_prefix = "k = "
) {
    if (is.null(aug_factors)) {
        aug_factors <- union(
            rand_ilr_half$augmentation_factor,
            rod_half$augmentation_factor
        )
    } else {
        aug_factors <- sort(unique(aug_factors))
    }

    if (length(aug_factors) == 0) {
        warning("No augmentation_factor values detected in the supplied data.")
        return(invisible(NULL))
    }

    for (k in aug_factors) {
        cat("### ", heading_prefix, k, "\n\n", sep = "")
        print(
            mean_diff_heatmap(
                rand_ilr_half = rand_ilr_half,
                rand_ilr_max = rand_ilr_max,
                rod_half = rod_half,
                rod_max = rod_max,
                bench_half = bench_half,
                bench_max = bench_max,
                aug_factor = k,
                plot_metric = plot_metric
            )
        )
        cat("\n\n")
    }

    invisible(NULL)
}

# File with helper functions for the quarto file "methods_comparison" -
# dashboard taht makes the direct comparison bewteen different augmentation methods

# Function to prepare all results for plotting
# Prepare each results independently, merge all datasets into one df
# unnest perf_metric and filter(.metric == sel_metric)
# return one merged df with filtered metric only to sel_metric

prepare_data <- function(
    rand_ilr_half,
    rand_ilr_max,
    benchmark_max,
    benchmark_half,
    rod_half,
    rod_max,
    plot_metric = c("roc_auc", "misclassification_rate")
) {
    cols_to_keep <- c(
        "data_id",
        "model",
        "label",
        "augmentation_factor",
        "perf_metrics",
        "pseudo_count"
    )

    process_rand_ilr <- function(df) {
        df |>
            mutate(
                density = as.character(density),
                density = if_else(
                    is.na(density),
                    "Unit density",
                    paste("density", density)
                ),
                label = paste(augmentation, density)
            ) |>
            select(all_of(cols_to_keep))
    }

    process_benchmark <- function(df) {
        df |>
            mutate(
                label = paste("Benchmark", as.character(transform)),
                # I add augmentation factor such that later I can merge all dfs
                augmentation_factor = 0
            ) |>
            select(all_of(cols_to_keep))
    }

    process_rod <- function(df) {
        df |>
            rename(label = augmentation) |>
            select(all_of(cols_to_keep))
    }

    combined <- bind_rows(
        process_rand_ilr(rand_ilr_half),
        process_rand_ilr(rand_ilr_max),
        process_benchmark(benchmark_half),
        process_benchmark(benchmark_max),
        process_rod(rod_half),
        process_rod(rod_max)
    )

    if (plot_metric != "misclassification_rate") {
        combined |>
            unnest(perf_metrics) |>
            filter(.metric == plot_metric)
    } else {
        combined |>
            unnest(perf_metrics) |>
            filter(.metric == "accuracy") |>
            mutate(
                misclas = 1 - .estimate,
                .metric = "Misclassification Rate"
            ) |>
            select(-c(".estimate")) |>
            rename(.estimate = misclas)
    }
}

# Function that creates the boxplot of selected perfromance metraic for all models
# and augmentation techniques for a selected augmentation factor

plot_metric_facets <- function(
    rand_ilr_half,
    rand_ilr_max,
    benchmark_max,
    benchmark_half,
    rod_half,
    rod_max,
    plot_metric,
    aug_factor
) {
    if (missing(plot_metric) || length(plot_metric) != 1L) {
        stop("`plot_metric` must be a single character value.", call. = FALSE)
    }

    base_data <- prepare_data(
        rand_ilr_half = rand_ilr_half,
        rand_ilr_max = rand_ilr_max,
        benchmark_max = benchmark_max,
        benchmark_half = benchmark_half,
        rod_half = rod_half,
        rod_max = rod_max,
        plot_metric = plot_metric
    )
    desired_levels <- c(
        "Benchmark - Prop.",
        "Benchmark - Stan. ILR",
        "Aitchison Mixup",
        "Comp. Cutmix",
        "Comp. Feature Dropout",
        "Aug. in p - Unit density",
        "Aug. in p - density 0.1",
        "Aug. in p - density 0.5",
        "Aug. in n - Unit density",
        "Aug. in n - density 0.1",
        "Aug. in n - density 0.5"
    )

    plot_data <- base_data |>
        filter(augmentation_factor %in% unique(c(0, aug_factor))) |>
        mutate(
            label = str_replace(
                label,
                "^aug_in_n\\s*(.*)$",
                "Aug. in n - \\1"
            ),
            label = str_replace(
                label,
                "^aug_in_p\\s*(.*)$",
                "Aug. in p - \\1"
            ),
            label = case_when(
                label == "Benchmark proportion" ~ "Benchmark - Prop.",
                label == "Benchmark standard_ilr" ~ "Benchmark - Stan. ILR",
                label == "aitchison_mixup" ~ "Aitchison Mixup",
                label == "comp_cutmix" ~ "Comp. Cutmix",
                label == "comp_feature" ~ "Comp. Feature Dropout",
                TRUE ~ label
            ),
            label = str_replace(label, "\\s*-\\s*$", ""),
            label = factor(label, levels = desired_levels),
            data_id = factor(
                data_id,
                levels = sort(unique(as.numeric(data_id)))
            ),
            #label = factor(label, levels = sort(unique(label))),
            pseudo_count = case_when(
                pseudo_count == "max_lib_size" ~ "1 / Max lib. size",
                pseudo_count == "half" ~ "0.5",
                TRUE ~ pseudo_count
            ),
            pseudo_count = factor(
                pseudo_count,
                levels = c("1 / Max lib. size", "0.5")
            )
        )

    metric_label <- unique(plot_data$.metric)
    metric_label <- recode(
        metric_label,
        roc_auc = "ROC AUC",
        misclassification_rate = "Misclassification Rate",
        .default = metric_label
    )

    model <- unique(plot_data$model)

    if (model %in% c("lasso", "lasso_ilr")) {
        model <- "Logistic Regression with L1 Penalty"
    } else if (model == "xgboost") {
        model <- "XGBoost"
    } else {
        model <- model |> str_replace("_", " ") |> str_to_title()
    }

    bench_cut <- max(which(
        levels(plot_data$label) %in%
            c("Benchmark - Prop.", "Benchmark - Stan. ILR")
    ))
    rod_cut <- which(levels(plot_data$label) == "Comp. Feature Dropout")

    ggplot(
        plot_data,
        aes(x = label, y = .estimate, fill = pseudo_count)
    ) +
        geom_boxplot() +
        geom_vline(
            xintercept = c(bench_cut + 0.5, rod_cut + 0.5),
            linetype = "dashed",
            colour = "grey60"
        ) +
        facet_wrap(~data_id) +
        scale_fill_viridis(discrete = TRUE) +
        theme_bw() +
        labs(
            x = NULL,
            y = metric_label,
            fill = "Pseudo-count",
            title = paste(
                "Comparison of",
                metric_label,
                "for",
                model,
                "Across Augmentation Techniques and Datasets"
            ),
            subtitle = paste("Augmentation Factor:", aug_factor)
        ) +
        coord_cartesian(clip = "off") +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 12),
            panel.grid.major.x = element_blank(),
            plot.margin = margin(l = 16, r = 12, t = 10, b = 18),
        ) +
        ylim(0, 1)
}

# Function that creates tabsets with a selected perfromance metric of a given model
# across specified aug. factors
render_metric_facets_tabset <- function(
    rand_ilr_half,
    rand_ilr_max,
    benchmark_max,
    benchmark_half,
    rod_half,
    rod_max,
    plot_metric,
    aug_factors = c(2, 3, 4, 5)
) {
    aug_factors <- sort(unique(aug_factors))

    for (k in aug_factors) {
        cat("### k = ", k, "\n\n", sep = "")
        print(
            plot_metric_facets(
                rand_ilr_half = rand_ilr_half,
                rand_ilr_max = rand_ilr_max,
                benchmark_max = benchmark_max,
                benchmark_half = benchmark_half,
                rod_half = rod_half,
                rod_max = rod_max,
                plot_metric = plot_metric,
                aug_factor = k
            )
        )
        cat("\n\n")
    }

    invisible(NULL)
}


make_metric_highlight_table <- function(
    rand_ilr_half,
    rand_ilr_max,
    benchmark_max,
    benchmark_half,
    rod_half,
    rod_max,
    plot_metric,
    aug_factor,
    sel_pseudo_count,
    digits = 3,
    format = c("html", "latex")
) {
    format <- match.arg(format)

    prepared <- prepare_data(
        rand_ilr_half = rand_ilr_half,
        rand_ilr_max = rand_ilr_max,
        benchmark_max = benchmark_max,
        benchmark_half = benchmark_half,
        rod_half = rod_half,
        rod_max = rod_max,
        plot_metric = plot_metric
    )

    filtered <- prepared |>
        filter(
            pseudo_count == sel_pseudo_count,
            augmentation_factor %in% unique(c(0, aug_factor))
        ) |>
        mutate(
            label = stringr::str_replace(
                label,
                "^aug_in_n\\s*(.*)$",
                "Aug. in n - \\1"
            ),
            label = stringr::str_replace(
                label,
                "^aug_in_p\\s*(.*)$",
                "Aug. in p - \\1"
            ),
            label = case_when(
                label == "Benchmark proportion" ~ "Benchmark - Prop.",
                label == "Benchmark standard_ilr" ~ "Benchmark - Stan. ILR",
                label == "aitchison_mixup" ~ "Aitchison Mixup",
                label == "comp_cutmix" ~ "Comp. Cutmix",
                label == "comp_feature" ~ "Comp. Feature Dropout",
                TRUE ~ label
            ),
            label = stringr::str_replace(label, "\\s*-\\s*$", "")
        )

    desired_order <- c(
        "Benchmark - Prop.",
        "Benchmark - Stan. ILR",
        "Aitchison Mixup",
        "Comp. Cutmix",
        "Comp. Feature Dropout",
        "Aug. in p - Unit density",
        "Aug. in p - density 0.1",
        "Aug. in p - density 0.5",
        "Aug. in n - Unit density",
        "Aug. in n - density 0.1",
        "Aug. in n - density 0.5"
    )

    summary_wide <- filtered |>
        group_by(data_id, label) |>
        summarise(
            mean_value = mean(.estimate, na.rm = TRUE),
            .groups = "drop"
        ) |>
        pivot_wider(names_from = label, values_from = mean_value) |>
        mutate(
            augmentation_factor = aug_factor,
            .metric = plot_metric,
            data_id = as.integer(data_id)
        ) |>
        arrange(data_id) |>
        relocate(augmentation_factor, .metric, data_id)

    formatted <- format_summary_table(
        summary_df = summary_wide,
        digits = digits,
        format = format
    )

    available_cols <- setdiff(
        names(formatted),
        c("augmentation_factor", ".metric", "data_id")
    )
    ordered_cols <- c(
        intersect(desired_order, available_cols),
        setdiff(available_cols, desired_order)
    )

    display_df <- formatted |>
        select(data_id, all_of(ordered_cols)) |>
        rename(Dataset = data_id)

    align <- c("l", rep("c", length(ordered_cols)))

    kableExtra::kbl(
        display_df,
        format = format,
        booktabs = format == "latex",
        escape = FALSE,
        align = align,
        digits = digits
    )
}

# Function that creates the tables with mean perfromance metric for each aug_factors,
# highlights the best perfromance in each row, then creates the summary table
# which shows for each augmentation factor the number of times a given group
# (groups: benchmark, Rodriguez methods, random ilr methods) turned out to lead to the best perfromance
# summary table column draw - number of times when the best perfromance was achieved by 2 different strategies
# when 2 or more entrie in a row are highlighted but they belong to the same group, then it's counted as +1 for that group
summarise_best_by_group <- function(
    rand_ilr_half,
    rand_ilr_max,
    benchmark_max,
    benchmark_half,
    rod_half,
    rod_max,
    plot_metric,
    sel_pseudo_count,
    aug_factors = c(2, 3, 4, 5),
    digits = 3,
    format = c("html", "latex")
) {
    format <- match.arg(format)
    aug_factors <- sort(unique(aug_factors))

    tables <- set_names(
        map(
            aug_factors,
            ~ make_metric_highlight_table(
                rand_ilr_half = rand_ilr_half,
                rand_ilr_max = rand_ilr_max,
                benchmark_max = benchmark_max,
                benchmark_half = benchmark_half,
                rod_half = rod_half,
                rod_max = rod_max,
                plot_metric = plot_metric,
                aug_factor = .x,
                sel_pseudo_count = sel_pseudo_count,
                digits = digits,
                format = format
            )
        ),
        paste0("k_", aug_factors)
    )

    base_data <- prepare_data(
        rand_ilr_half = rand_ilr_half,
        rand_ilr_max = rand_ilr_max,
        benchmark_max = benchmark_max,
        benchmark_half = benchmark_half,
        rod_half = rod_half,
        rod_max = rod_max,
        plot_metric = plot_metric
    ) |>
        filter(pseudo_count == sel_pseudo_count) |>
        mutate(
            group = case_when(
                label %in%
                    c(
                        "Benchmark proportion",
                        "Benchmark standard_ilr"
                    ) ~ "Benchmark",
                label %in%
                    c(
                        "comp_cutmix",
                        "comp_feature",
                        "aitchison_mixup"
                    ) ~ "Rodriguez",
                TRUE ~ "Random ILR"
            )
        )

    direction <- metric_direction(plot_metric)

    summary_list <- map(aug_factors, function(k) {
        data_k <- base_data |>
            filter(augmentation_factor %in% c(0, k))

        if (nrow(data_k) == 0) {
            return(tibble(
                augmentation_factor = k,
                Benchmark = 0L,
                Rodriguez = 0L,
                `Random ILR` = 0L,
                Draw = 0L
            ))
        }

        summary_long <- data_k |>
            group_by(data_id, label, group) |>
            summarise(
                mean_val = mean(.estimate, na.rm = TRUE),
                .groups = "drop"
            ) |>
            mutate(mean_val = round(mean_val, digits))

        label_groups <- summary_long |>
            distinct(label, group) |>
            deframe()

        value_wide <- summary_long |>
            select(data_id, label, mean_val) |>
            pivot_wider(
                names_from = label,
                values_from = mean_val
            )

        value_cols <- setdiff(names(value_wide), "data_id")
        counts <- c(
            Benchmark = 0L,
            Rodriguez = 0L,
            `Random ILR` = 0L,
            Draw = 0L
        )

        if (nrow(value_wide) > 0 && length(value_cols) > 0) {
            for (i in seq_len(nrow(value_wide))) {
                row_vals <- unlist(
                    value_wide[i, value_cols, drop = FALSE],
                    use.names = TRUE
                )
                valid <- !is.na(row_vals)
                if (!any(valid)) {
                    next
                }

                target <- if (direction == "max") {
                    max(row_vals[valid])
                } else {
                    min(row_vals[valid])
                }

                winners <- names(row_vals)[
                    valid & near(row_vals, target)
                ]

                if (length(winners) == 1) {
                    grp <- label_groups[[winners]]
                    if (!is.null(grp) && !is.na(grp)) {
                        counts[[grp]] <- counts[[grp]] + 1L
                    } else {
                        counts[["Draw"]] <- counts[["Draw"]] + 1L
                    }
                } else if (length(winners) > 1) {
                    grp_candidates <- unique(stats::na.omit(label_groups[
                        winners
                    ]))
                    if (length(grp_candidates) == 1) {
                        counts[[grp_candidates]] <- counts[[grp_candidates]] +
                            1L
                    } else {
                        counts[["Draw"]] <- counts[["Draw"]] + 1L
                    }
                }
            }
        }

        tibble(
            augmentation_factor = k,
            Benchmark = counts[["Benchmark"]],
            Rodriguez = counts[["Rodriguez"]],
            `Random ILR` = counts[["Random ILR"]],
            Draw = counts[["Draw"]]
        )
    })

    summary <- bind_rows(summary_list)

    list(
        tables = tables,
        summary = summary
    )
}

# Function that creates the table with mean performance metric for both pseudo-counts,
# prints nicely formated table and the summary table specyfying how often each of the strategy
# resulted in the best perfromance for a given augmentation factor
render_mean_metric_sections <- function(
    rand_ilr_half,
    rand_ilr_max,
    benchmark_max,
    benchmark_half,
    rod_half,
    rod_max,
    plot_metric
) {
    pseudo_info <- c(
        half = "Pseudo-count = 1/2",
        max_lib_size = "Pseudo-count = 1/max library size"
    )

    purrr::iwalk(
        pseudo_info,
        function(title, pseudo_id) {
            cat("#### ", title, "\n\n", sep = "")

            result <- summarise_best_by_group(
                rand_ilr_half = rand_ilr_half,
                rand_ilr_max = rand_ilr_max,
                benchmark_max = benchmark_max,
                benchmark_half = benchmark_half,
                rod_half = rod_half,
                rod_max = rod_max,
                plot_metric = plot_metric,
                sel_pseudo_count = pseudo_id
            )

            cat("::: panel-tabset\n")
            purrr::iwalk(
                result$tables,
                function(tbl, nm) {
                    label <- stringr::str_replace(nm, "^k_", "k = ")
                    cat("### ", label, "\n\n", sep = "")

                    tbl |>
                        kableExtra::kable_styling(
                            bootstrap_options = c(
                                "striped",
                                "hover",
                                "condensed"
                            ),
                            full_width = FALSE,
                            font_size = 13
                        ) |>
                        kableExtra::row_spec(0, bold = TRUE) |>
                        print()

                    cat("\n\n")
                }
            )
            cat(":::\n\n")

            cat(
                "The table below shows, for this pseudo-count, how often ",
                "each group achieved the best mean performance. Results are ",
                "reported separately for each augmentation factor.\n\n",
                sep = ""
            )

            result$summary |>
                dplyr::rename(`Augmentation Factor` = augmentation_factor) |>
                kableExtra::kbl(
                    format = "html",
                    col.names = c(
                        "Augmentation Factor",
                        "Benchmark",
                        "Rodriguez",
                        "Random ILR",
                        "Draw"
                    ),
                    align = c("c", rep("c", 4))
                ) |>
                kableExtra::kable_styling(
                    bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = FALSE,
                    font_size = 13
                ) |>
                kableExtra::row_spec(0, bold = TRUE) |>
                print()

            cat("\n\n")
        }
    )

    invisible(NULL)
}

# Function that for 2 df with the results of random ILR and Rodriguez
#  augmentation techniques for 1 model computes mean and st.dev. of
# gain as compared to benchmark in the selected perfromance metric and
# aug_factor
summarise_model_metrics <- function(
    rand_ilr_res,
    rod_res,
    benchmark,
    sel_aug_factor,
    plot_metric
) {
    summarise_one <- function(
        df,
        grouping_cols,
        benchmark_transform
    ) {
        metric_name <- if (
            plot_metric %in%
                c(
                    "misclassification_rate",
                    "missclassification_rate"
                )
        ) {
            "accuracy"
        } else {
            plot_metric
        }

        metric_tbl <- df |>
            filter(augmentation_factor == sel_aug_factor) |>
            tidyr::unnest(perf_metrics) |>
            filter(.metric == metric_name) |>
            mutate(
                metric_value = if (
                    plot_metric %in%
                        c(
                            "misclassification_rate",
                            "missclassification_rate"
                        )
                ) {
                    1 - .estimate
                } else {
                    .estimate
                }
            )

        # Prepare benchmark values to compare against
        benchmark_tbl <- benchmark |>
            filter(
                transform == benchmark_transform,
                model %in% unique(metric_tbl$model)
            ) |>
            tidyr::unnest(perf_metrics) |>
            filter(.metric == metric_name) |>
            mutate(
                benchmark_value = if (
                    plot_metric %in%
                        c(
                            "misclassification_rate",
                            "missclassification_rate"
                        )
                ) {
                    1 - .estimate
                } else {
                    .estimate
                }
            )

        join_cols <- intersect(
            c("data_id", "split"),
            names(metric_tbl)
        )
        benchmark_tbl <- benchmark_tbl |>
            select(all_of(join_cols), benchmark_value)

        comparison_tbl <- metric_tbl |>
            left_join(benchmark_tbl, by = join_cols) |>
            mutate(gain = metric_value - benchmark_value)

        comparison_tbl |>
            group_by(across(all_of(grouping_cols))) |>
            summarise(
                mean_metric = mean(gain, na.rm = TRUE),
                sd_metric = sd(gain, na.rm = TRUE),
                .groups = "drop"
            )
    }

    bind_rows(
        summarise_one(
            rand_ilr_res,
            grouping_cols = c("data_id", "augmentation", "density"),
            benchmark_transform = "standard_ilr"
        ),
        summarise_one(
            rod_res,
            grouping_cols = c("data_id", "augmentation"),
            benchmark_transform = "proportion"
        )
    )
}

# Create scatter plot showing mean gain in a selected perfromance metric and augmentation factor
# Dots are colored based on the augmentation technique group:
#    Rodriguez, aug in n, aug in p (no distinction for density or exact Rodriguez method)
plot_augmentation_gain <- function(
    rand_ilr_res,
    rod_res,
    benchmark,
    data_dim,
    sel_aug_factor = 2,
    plot_metric = "accuarcy"
) {
    data_dim_tbl <- data_dim
    if (
        "Task" %in% names(data_dim_tbl) && !"data_id" %in% names(data_dim_tbl)
    ) {
        data_dim_tbl <- rename(data_dim_tbl, data_id = Task)
    }
    data_dim_tbl <- data_dim_tbl |>
        mutate(n_p_ratio = n / p)

    # Mean difference in selected metric
    data_prep <- summarise_model_metrics(
        rand_ilr_res = rand_ilr_res,
        rod_res = rod_res,
        benchmark = benchmark,
        sel_aug_factor = sel_aug_factor,
        plot_metric = plot_metric
    )
    # Add n/p ratio, Prepare color labels
    plot_data <- data_prep |>
        mutate(data_id = as.integer(data_id)) |>
        left_join(data_dim_tbl, by = "data_id") |>
        mutate(
            color_label = case_when(
                augmentation %in%
                    c(
                        "aitchison_mixup",
                        "comp_cutmix",
                        "comp_feature"
                    ) ~ "Rodriguez Methods",
                augmentation == "aug_in_n" ~ "randomILR Augmentation in n",
                augmentation == "aug_in_p" ~ "randomILR Augmentation in p",
                .default = as.character(augmentation)
            )
        )

    model_value <- unique(rand_ilr_res$model)
    model_label <- case_when(
        model_value == "xgboost" ~ "XGBoost",
        model_value == "lasso" ~ "Logistic Regression with L1 Penalty",
        model_value == "random_forest" ~ "Random Forest",
        TRUE ~ str_to_title(ifelse(is.na(model_value), "Model", model_value))
    )

    plot_metric_name <- case_when(
        plot_metric == "misclassification_rate" ~ "Misclassification Rate",
        plot_metric == "roc_auc" ~ "ROC AUC",
        .default = str_to_title(plot_metric)
    )

    ggplot(
        plot_data,
        aes(x = n_p_ratio, y = mean_metric, color = color_label)
    ) +
        geom_point(size = 7, alpha = 0.7) +
        geom_hline(yintercept = 0, color = "grey60", linetype = "dashed") +
        scale_color_viridis_d() +
        theme_bw() +
        labs(
            title = paste(
                "Mean",
                plot_metric_name,
                "Gain from Augmentation for",
                model_label
            ),
            subtitle = paste("Augmentation Factor:", sel_aug_factor),
            x = "n/p Ratio",
            y = plot_metric_name,
            color = NULL
        ) +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 12)
        ) +
        ylim(-0.15, 0.15)
}

# Display scatter plot of mean gain for each aug_factor in separate tab
render_aug_factor_tabset <- function(
    rand_ilr_res,
    rod_res,
    benchmark,
    data_dim,
    plot_metric = "accuarcy",
    aug_factors = c(2:5),
    heading_prefix = "k ="
) {
    for (k in aug_factors) {
        cat("### ", heading_prefix, k, "\n\n", sep = "")
        print(
            plot_augmentation_gain(
                rand_ilr_res = rand_ilr_res,
                rod_res = rod_res,
                benchmark = benchmark,
                data_dim = data_dim,
                sel_aug_factor = k,
                plot_metric = plot_metric
            )
        )
        cat("\n\n")
    }
}

# Function to compute pairwise differencess between performance metrics
# obtained by random ilr and Rodriguez augmentation strategies
# For a given aug factor, data id, data split and perfromance metric: Random ILR - Rodriguez
compute_pairwise_metric_diff <- function(
    rand_ilr_res,
    rod_res,
    plot_metric,
    sel_aug_factor
) {
    if (missing(plot_metric) || length(plot_metric) != 1L) {
        stop("`plot_metric` must be a single character value.", call. = FALSE)
    }
    if (missing(sel_aug_factor) || length(sel_aug_factor) != 1L) {
        stop("`sel_aug_factor` must be a single numeric value.", call. = FALSE)
    }

    metric_name <- if (
        plot_metric %in%
            c(
                "misclassification_rate",
                "missclassification_rate"
            )
    ) {
        "accuracy"
    } else {
        plot_metric
    }
    convert_metric <- plot_metric %in%
        c(
            "misclassification_rate",
            "missclassification_rate"
        )

    rand_tbl <- rand_ilr_res |>
        dplyr::filter(augmentation_factor == sel_aug_factor) |>
        tidyr::unnest(perf_metrics) |>
        dplyr::filter(.metric == metric_name) |>
        dplyr::mutate(
            metric_value = if (convert_metric) {
                1 - .estimate
            } else {
                .estimate
            }
        )

    if (!"augmentation" %in% names(rand_tbl)) {
        stop(
            "`rand_ilr_res` must contain an `augmentation` column.",
            call. = FALSE
        )
    }

    rand_tbl <- rand_tbl |>
        dplyr::mutate(augmentation = as.character(augmentation))

    rand_tbl <- rand_tbl |>
        dplyr::mutate(
            density = as.character(density),
            rand_method = dplyr::if_else(
                is.na(density) | density == "",
                paste0(augmentation, "_unit_density"),
                paste(augmentation, density, sep = "_density_")
            )
        )

    rod_tbl <- rod_res |>
        dplyr::filter(augmentation_factor == sel_aug_factor) |>
        tidyr::unnest(perf_metrics) |>
        dplyr::filter(.metric == metric_name) |>
        dplyr::mutate(
            metric_value = if (convert_metric) {
                1 - .estimate
            } else {
                .estimate
            }
        )

    if (!"augmentation" %in% names(rod_tbl)) {
        stop("`rod_res` must contain an `augmentation` column.", call. = FALSE)
    }

    rod_tbl <- rod_tbl |>
        dplyr::mutate(
            augmentation = as.character(augmentation),
            rod_method = augmentation
        )

    candidate_keys <- c(
        "data_id",
        "split",
        "model"
    )
    join_cols <- intersect(candidate_keys, names(rand_tbl))
    join_cols <- intersect(join_cols, names(rod_tbl))

    if (!"data_id" %in% join_cols) {
        stop("Input data must share a `data_id` column.", call. = FALSE)
    }

    dplyr::inner_join(
        rod_tbl |>
            dplyr::select(
                dplyr::all_of(join_cols),
                rod_method,
                rod_estimate = metric_value
            ),
        rand_tbl |>
            dplyr::select(
                dplyr::all_of(join_cols),
                rand_method,
                rand_estimate = metric_value
            ),
        by = join_cols,
        relationship = "many-to-many"
    ) |>
        dplyr::mutate(
            augmentation_factor = sel_aug_factor,
            estimate_diff = rand_estimate - rod_estimate
        ) |>
        dplyr::select(
            dplyr::all_of(join_cols),
            augmentation_factor,
            rod_method,
            rand_method,
            rod_estimate,
            rand_estimate,
            estimate_diff
        ) |>
        dplyr::arrange(
            !!!rlang::syms(join_cols),
            rod_method,
            rand_method
        )
}

# Heatmap showing the mean difference bewteen random ILR and Rodriguez augmentation methods
# The scale color of heatmap is designed such that "positive" change (positive for the first
# of models, so in this case one of random ilr methods) of a given metric is always in BLUE
# whereas the negative change of this metric is in RED
plot_pairwise_metric_heatmap <- function(
    rand_ilr_res,
    rod_res,
    plot_metric,
    sel_aug_factor
) {
    pairwise_diff <- compute_pairwise_metric_diff(
        rand_ilr_res = rand_ilr_res,
        rod_res = rod_res,
        plot_metric = plot_metric,
        sel_aug_factor = sel_aug_factor
    )

    format_rod_method <- function(x) {
        prettified <- dplyr::case_match(
            x,
            "aitchison_mixup" ~ "Aitchison Mixup",
            "comp_cutmix" ~ "Comp. CutMix",
            "comp_feature" ~ "Comp. Feature Dropout",
            .default = stringr::str_to_title(stringr::str_replace_all(
                x,
                "_",
                " "
            ))
        )
        prettified
    }

    format_rand_method <- function(x) {
        purrr::map_chr(
            x,
            \(val) {
                if (is.na(val) || val == "") {
                    return(NA_character_)
                }

                if (stringr::str_detect(val, "^aug_in_n")) {
                    base <- "Aug. in n"
                } else if (stringr::str_detect(val, "^aug_in_p")) {
                    base <- "Aug. in p"
                } else {
                    return(stringr::str_to_title(stringr::str_replace_all(
                        val,
                        "_",
                        " "
                    )))
                }

                density_match <- stringr::str_match(val, "density_(.*)$")[, 2]
                if (!is.na(density_match) && density_match != "") {
                    return(paste(
                        base,
                        paste("density", density_match),
                        sep = ", "
                    ))
                }

                if (stringr::str_detect(val, "unit_density$")) {
                    return(paste(base, "unit density", sep = ", "))
                }

                base
            }
        )
    }
    model_value <- unique(rand_ilr_res$model)
    model_label <- case_when(
        model_value == "xgboost" ~ "XGBoost",
        model_value == "lasso" ~ "Logistic Regression with L1 Penalty",
        model_value == "random_forest" ~ "Random Forest",
        TRUE ~ str_to_title(ifelse(is.na(model_value), "Model", model_value))
    )

    heatmap_data <- pairwise_diff |>
        dplyr::group_by(data_id, rod_method, rand_method) |>
        dplyr::summarise(
            mean_diff = mean(estimate_diff, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            rod_method = format_rod_method(rod_method),
            rand_method = format_rand_method(rand_method)
        ) |>
        dplyr::mutate(
            label = paste(rand_method, "-", rod_method)
        )

    metric_label <- if (identical(plot_metric, "roc_auc")) {
        "ROC AUC"
    } else {
        plot_metric |> str_replace_all("_", " ") |> str_to_title()
    }

    label_levels <- heatmap_data |>
        distinct(label) |>
        dplyr::arrange(label) |>
        dplyr::pull(label)

    max_abs <- max(abs(heatmap_data$mean_diff), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) {
        max_abs <- 1
    }

    heatmap_data <- heatmap_data |>
        dplyr::mutate(
            data_id = factor(
                as.numeric(data_id),
                levels = sort(unique(as.numeric(data_id)))
            ),
            label = factor(label, levels = label_levels)
        )
    # Scale color depends on the metric(good means something different for 2 metrics)
    # negative misclassification rate is good, whereas for roc auc positive change is good
    if (
        plot_metric %in% c("misclassification_rate", "missclassification_rate")
    ) {
        col_scale <- list(negative_chg = "#2b83ba", positive_chg = "#d7191c")
    } else {
        col_scale <- list(negative_chg = "#d7191c", positive_chg = "#2b83ba")
    }

    ggplot2::ggplot(
        heatmap_data,
        ggplot2::aes(x = data_id, y = label, fill = mean_diff)
    ) +
        ggplot2::geom_tile(color = "grey85") +
        ggplot2::scale_fill_gradient2(
            limits = c(-max_abs, max_abs),
            low = col_scale$negative_chg,
            mid = "white",
            high = col_scale$positive_chg,
            midpoint = 0,
            oob = scales::squish
        ) +
        ggplot2::labs(
            x = "Data ID",
            y = NULL,
            fill = "Mean diff.",
            title = paste(
                "Pairwise Mean Performance Difference of",
                metric_label,
                "for",
                model_label
            ),
            subtitle = paste(
                "Augmentation factor:",
                sel_aug_factor
            )
        ) +
        ggplot2::theme_bw() +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 12),
            plot.title.position = "plot",
            plot.margin = margin(t = 12, r = 12, b = 12, l = 12)
        )
}

# Plot heatmaps for a given pseudo-count in seperate tabsets
render_metric_diff_heatmap_tabset <- function(
    rand_ilr_res,
    rod_res,
    plot_metric,
    aug_factors = c(2, 3, 4, 5)
) {
    aug_factors <- sort(unique(aug_factors))

    for (k in aug_factors) {
        cat("### k = ", k, "\n\n", sep = "")
        print(
            plot_pairwise_metric_heatmap(
                rand_ilr_res,
                rod_res,
                plot_metric,
                sel_aug_factor = k
            )
        )
        cat("\n\n")
    }

    invisible(NULL)
}

# Purpose of the plot - check the impact of the density of skew-symmetric matrix on the perfromance of
# model trained on the data augmented in p across augemntation factors
# Create the boxplot showing the plot_metric for the model trained on the data augmented in p
# x axis - datasets, y - plot_metric, color - density
# model_name and pseudo_count are only used to create meaningfull title and subtitle
plot_aug_in_p_density_boxplot <- function(
    rand_ilr_res,
    plot_metric,
    model_name,
    pseudo_count
) {
    if (missing(plot_metric) || length(plot_metric) != 1L) {
        stop("`plot_metric` must be a single character value.", call. = FALSE)
    }

    metric_name <- if (
        plot_metric %in%
            c(
                "misclassification_rate",
                "missclassification_rate"
            )
    ) {
        "accuracy"
    } else {
        plot_metric
    }
    convert_metric <- plot_metric %in%
        c(
            "misclassification_rate",
            "missclassification_rate"
        )

    plot_data <- rand_ilr_res |>
        filter(augmentation == "aug_in_p") |>
        tidyr::unnest(perf_metrics) |>
        filter(.metric == metric_name) |>
        mutate(
            metric_value = if (convert_metric) {
                1 - .estimate
            } else {
                .estimate
            },
            density = case_when(
                is.na(density) ~ "Unit density",
                TRUE ~ paste("Density", density)
            ),
            density = factor(
                density,
                levels = c("Unit density", "Density 0.1", "Density 0.5")
            ),
            augmentation_factor = factor(
                augmentation_factor,
                levels = sort(unique(augmentation_factor))
            ),
            data_id = factor(
                as.numeric(data_id),
                levels = sort(unique(as.numeric(data_id)))
            )
        )

    metric_label <- case_when(
        plot_metric == "roc_auc" ~ "ROC AUC",
        plot_metric %in%
            c(
                "misclassification_rate",
                "missclassification_rate"
            ) ~ "Misclassification Rate",
        TRUE ~ plot_metric |> str_replace_all("_", " ") |> str_to_title()
    )

    ggplot(
        plot_data,
        aes(
            x = data_id,
            y = metric_value,
            fill = density
        )
    ) +
        geom_boxplot(
            outlier.alpha = 0.5,
            width = 0.6
        ) +
        facet_wrap(
            ~augmentation_factor,
            ncol = 2
        ) +
        scale_fill_viridis(discrete = TRUE) +
        labs(
            x = "Data ID",
            y = metric_label,
            fill = "Density",
            title = paste(
                "Impact of Density on",
                model_name,
                metric_label,
                "- Data Augmented in p"
            ),
            subtitle = paste("Split by Augmentation Factor;", pseudo_count)
        ) +
        theme_bw() +
        theme(
            legend.position = "bottom",
            plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12, hjust = 1),
            axis.text.y = element_text(size = 12),
            strip.text = element_text(size = 12)
        ) +
        ylim(0, 1)
}

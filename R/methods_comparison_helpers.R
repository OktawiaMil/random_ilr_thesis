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

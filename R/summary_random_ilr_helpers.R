# Helper functions for the Quarto docs summarising the results
# of random ILR augmentation methods

# Prepare performance metrics for summary tables:
# - convert accuracy into misclassification_rate (1 - accuracy)
# - keep only the metrics of interest (ROC AUC and misclassification rate)
# - filter from benchmark results relevant results
# (only transform == "standard_ilr")
prepare_summary_metrics_ilr <- function(data) {
    if ("transform" %in% names(data) && !"augmentation" %in% names(data)) {
        # We work on benchmark results
        data <- data |>
            filter(transform == "standard_ilr") |>
            rename(augmentation = transform) |>
            mutate(density = 0)
    }

    preserved_cols <- intersect(
        c("data_id", "augmentation", "augmentation_factor", "density"),
        names(data)
    )

    data |>
        select(all_of(preserved_cols), perf_metrics) |>
        unnest(perf_metrics) |>
        mutate(
            source_metric = .metric,
            .estimate = if_else(
                source_metric == "accuracy",
                1 - .estimate,
                .estimate
            ),
            .metric = case_when(
                source_metric == "accuracy" ~ "misclassification_rate",
                TRUE ~ source_metric
            )
        ) |>
        filter(.metric %in% c("roc_auc", "misclassification_rate")) |>
        select(-any_of(c("source_metric", ".estimator")))
}


# Internal helper: convert raw density values to labels used in tables
normalize_density_label <- function(density) {
    density_chr <- as.character(density)
    case_when(
        is.na(density_chr) ~ "Def.",
        TRUE ~ density_chr
    )
}

# Create the table with the mean value of the selected performance metric
# for a given augmentation factor and the selected model and its benchmark,
# round the means to digits decimal places
summarise_metric_table_ilr <- function(
    benchmark,
    aug_results,
    aug_factor,
    perf_metric,
    digits = 3
) {
    bench_tbl <- prepare_summary_metrics_ilr(benchmark) |>
        mutate(
            augmentation_factor = aug_factor,
            augmentation_group = "bench",
            density_label = NA_character_
        )

    aug_tbl <- prepare_summary_metrics_ilr(aug_results) |>
        filter(augmentation_factor == aug_factor) |>
        mutate(
            augmentation_group = case_when(
                augmentation == "aug_in_n" ~ "n",
                augmentation == "aug_in_p" ~ "p",
                TRUE ~ NA_character_
            ),
            density_label = normalize_density_label(density)
        )

    bench_summary <- bench_tbl |>
        filter(.metric == perf_metric) |>
        group_by(data_id) |>
        summarise(Benchmark = mean(.estimate, na.rm = TRUE), .groups = "drop")

    density_order <- c("Def.", "0.1", "0.5")
    density_keys <- c("Def", "0_1", "0_5")

    aug_summary <- aug_tbl |>
        filter(
            .metric == perf_metric,
            augmentation_group %in% c("n", "p")
        ) |>
        mutate(
            density_label = factor(
                density_label,
                levels = density_order
            ),
            density_key = case_when(
                density_label == "Def." ~ "Def",
                density_label == "0.1" ~ "0_1",
                density_label == "0.5" ~ "0_5",
                TRUE ~ str_replace_all(as.character(density_label), "\\.", "_")
            )
        ) |>
        group_by(data_id, augmentation_group, density_key) |>
        summarise(
            mean_metric = mean(.estimate, na.rm = TRUE),
            .groups = "drop"
        ) |>
        mutate(
            col_name = paste0("Aug_", augmentation_group, "_", density_key)
        ) |>
        select(data_id, col_name, mean_metric) |>
        pivot_wider(names_from = col_name, values_from = mean_metric)

    summary_wide <- bench_summary |>
        left_join(aug_summary, by = "data_id") |>
        mutate(
            augmentation_factor = aug_factor,
            .metric = perf_metric
        )

    expected_cols <- c(
        "Benchmark",
        paste0("Aug_n_", density_keys),
        paste0("Aug_p_", density_keys)
    )

    for (col in expected_cols) {
        if (!col %in% names(summary_wide)) {
            summary_wide[[col]] <- NA_real_
        }
    }

    summary_wide |>
        mutate(across(all_of(expected_cols), ~ round(.x, digits))) |>
        select(
            Dataset = data_id,
            Benchmark,
            Aug_n_Def = Aug_n_Def,
            Aug_n_0_1 = Aug_n_0_1,
            Aug_n_0_5 = Aug_n_0_5,
            Aug_p_Def = Aug_p_Def,
            Aug_p_0_1 = Aug_p_0_1,
            Aug_p_0_5 = Aug_p_0_5,
            augmentation_factor,
            .metric
        )
}

# Create summary table for all combinations of augmentation factor &
# performance metrics
build_summary_dataset_ilr <- function(benchmark, aug_results, digits = 3) {
    aug_factors <- aug_results |>
        pull(augmentation_factor) |>
        unique() |>
        sort()

    metrics <- c("roc_auc", "misclassification_rate")

    crossing(
        augmentation_factor = aug_factors,
        .metric = metrics
    ) |>
        mutate(
            data = map2(
                augmentation_factor,
                .metric,
                ~ summarise_metric_table_ilr(
                    benchmark = benchmark,
                    aug_results = aug_results,
                    aug_factor = .x,
                    perf_metric = .y,
                    digits = digits
                )
            )
        ) |>
        select(data) |>
        unnest(data)
}

# Format summary tables such that the best perfomance is bolded in each row
format_summary_table_ilr <- function(
    summary_df,
    digits = 3,
    format = c("html", "latex")
) {
    format <- match.arg(format)

    value_cols <- c(
        "Benchmark",
        "Aug_n_Def",
        "Aug_n_0_1",
        "Aug_n_0_5",
        "Aug_p_Def",
        "Aug_p_0_1",
        "Aug_p_0_5"
    )

    summary_df |>
        mutate(across(all_of(value_cols), ~ round(.x, digits))) |>
        rowwise() |>
        mutate(
            across(
                all_of(value_cols),
                ~ format_metric_value(
                    .x,
                    c_across(all_of(value_cols)),
                    .metric,
                    digits = digits,
                    format = format
                )
            )
        ) |>
        ungroup()
}

# Create interactive summary tables and format the table (central alignment)
create_summary_dashboard_random_ilr <- function(
    benchmark,
    aug_results,
    digits = 3,
    id_prefix = "random_ilr"
) {
    summary_wide <- build_summary_dataset_ilr(
        benchmark = benchmark,
        aug_results = aug_results,
        digits = digits
    )

    formatted <- format_summary_table_ilr(
        summary_wide,
        digits = digits,
        format = "html"
    )

    formatted <- formatted |>
        mutate(
            augmentation_factor = factor(
                augmentation_factor,
                levels = sort(unique(augmentation_factor))
            ),
            Metric = case_when(
                .metric == "roc_auc" ~ "ROC AUC",
                .metric == "misclassification_rate" ~ "Misclassification Rate",
                TRUE ~ .metric
            ),
            Dataset = as.integer(Dataset)
        ) |>
        arrange(augmentation_factor, Metric, Dataset)

    display_cols <- c(
        "Dataset",
        "Benchmark",
        "Aug_n_Def",
        "Aug_n_0_1",
        "Aug_n_0_5",
        "Aug_p_Def",
        "Aug_p_0_1",
        "Aug_p_0_5"
    )

    shared_table_data <- formatted |>
        select(all_of(display_cols))

    key_vec <- with(
        formatted,
        paste(augmentation_factor, Metric, Dataset, sep = "_")
    )

    shared_filters <- crosstalk::SharedData$new(
        formatted,
        key = key_vec,
        group = id_prefix
    )

    shared_table <- crosstalk::SharedData$new(
        shared_table_data,
        key = key_vec,
        group = id_prefix
    )

    table_container <- htmltools::tags$table(
        class = "display",
        htmltools::tags$thead(
            htmltools::tags$tr(
                htmltools::tags$th("Dataset", style = "text-align:center;"),
                htmltools::tags$th(
                    "Benchmark (Stan. ILR)",
                    style = "text-align:center;"
                ),
                htmltools::tags$th(
                    colspan = 3,
                    "Aug in n",
                    style = "text-align:center;"
                ),
                htmltools::tags$th(
                    colspan = 3,
                    "Aug in p",
                    style = "text-align:center;"
                )
            ),
            htmltools::tags$tr(
                htmltools::tags$th(
                    rowspan = 2,
                    "",
                    style = "text-align:center;"
                ),
                htmltools::tags$th(
                    rowspan = 2,
                    "",
                    style = "text-align:center;"
                ),
                htmltools::tags$th(
                    colspan = 6,
                    "Density of skew-symmetric matrix",
                    style = "text-align:center;"
                )
            ),
            htmltools::tags$tr(
                htmltools::tags$th("Def.", style = "text-align:center;"),
                htmltools::tags$th("0.1", style = "text-align:center;"),
                htmltools::tags$th("0.5", style = "text-align:center;"),
                htmltools::tags$th("Def.", style = "text-align:center;"),
                htmltools::tags$th("0.1", style = "text-align:center;"),
                htmltools::tags$th("0.5", style = "text-align:center;")
            )
        )
    )

    controls <- crosstalk::bscols(
        widths = c(6, 6),
        crosstalk::filter_select(
            paste0(id_prefix, "_metric"),
            "Performance metric",
            shared_filters,
            ~Metric
        ),
        crosstalk::filter_select(
            paste0(id_prefix, "_factor"),
            "Augmentation factor",
            shared_filters,
            ~augmentation_factor
        )
    )

    datatable_widget <- DT::datatable(
        shared_table,
        escape = FALSE,
        rownames = FALSE,
        container = table_container,
        options = list(
            dom = "tip",
            pageLength = 12,
            ordering = FALSE,
            autoWidth = TRUE,
            columnDefs = list(
                list(className = "dt-center", targets = "_all")
            )
        ),
        class = "display cell-border stripe"
    )

    htmltools::browsable(htmltools::tagList(controls, datatable_widget))
}

# Save summary tables of mean ROC AUC separately for each of the augmentation
# factor and model as the Latex code
save_metric_table_latex_random_ilr <- function(
    benchmark,
    aug_results,
    model_name,
    output_dir,
    metric = "roc_auc",
    metric_label = "ROC AUC",
    table_prefix = "random_ilr",
    digits = 3,
    caption_fun = NULL
) {
    if (is.null(caption_fun)) {
        caption_fun <- function(aug_factor, metric_label, model_name) {
            sprintf(
                "Mean %s of %s random ILR augmentations (augmentation factor = %s)",
                metric_label,
                toupper(model_name),
                aug_factor
            )
        }
    }

    aug_factors <- aug_results |>
        pull(augmentation_factor) |>
        unique() |>
        sort()

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    walk(aug_factors, function(aug_factor) {
        summary_wide <- summarise_metric_table_ilr(
            benchmark = benchmark,
            aug_results = aug_results,
            aug_factor = aug_factor,
            perf_metric = metric,
            digits = digits
        )

        formatted <- format_summary_table_ilr(
            summary_wide,
            digits = digits,
            format = "latex"
        )

        display_df <- formatted |>
            select(
                Dataset,
                Benchmark,
                Aug_n_Def,
                Aug_n_0_1,
                Aug_n_0_5,
                Aug_p_Def,
                Aug_p_0_1,
                Aug_p_0_5
            ) |>
            rename(
                `Dataset` = Dataset,
                `Benchmark` = Benchmark,
                `Aug_n_Def` = Aug_n_Def,
                `Aug_n_0_1` = Aug_n_0_1,
                `Aug_n_0_5` = Aug_n_0_5,
                `Aug_p_Def` = Aug_p_Def,
                `Aug_p_0_1` = Aug_p_0_1,
                `Aug_p_0_5` = Aug_p_0_5
            )

        col_names <- c(
            "",
            "",
            "Def.",
            "0.1",
            "0.5",
            "Def.",
            "0.1",
            "0.5"
        )

        table_obj <- kableExtra::kbl(
            display_df,
            format = "latex",
            booktabs = TRUE,
            escape = FALSE,
            align = rep("c", ncol(display_df)),
            col.names = col_names,
            caption = caption_fun(aug_factor, metric_label, model_name),
            label = sprintf(
                "tab:%s_%s_%s",
                table_prefix,
                metric,
                aug_factor
            )
        ) |>
            kableExtra::add_header_above(
                c(" " = 2, "Density of skew-symmetric matrix" = 6),
                escape = FALSE,
                align = "c"
            ) |>
            kableExtra::add_header_above(
                c(
                    "Dataset" = 1,
                    "Benchmark (Stan. ILR)" = 1,
                    "Aug in n" = 3,
                    "Aug in p" = 3
                ),
                escape = FALSE,
                align = "c"
            ) |>
            kableExtra::kable_styling(
                latex_options = c("hold_position")
            )

        file_name <- sprintf(
            "%s_%s_mean_%s_%s.tex",
            table_prefix,
            model_name,
            metric,
            aug_factor
        )

        file_path <- file.path(output_dir, file_name)
        kableExtra::save_kable(table_obj, file = file_path)
    })

    invisible(NULL)
}

# Function that creates the plot showing the impact of augmentation factor on
# selected performance metric, 1 plot in 1 tabset corresponds to 1 density of
# GHL matrix
render_aug_imp_tabs <- function(metric, aug_results) {
    unique_densities <- aug_results |> pull(density) |> unique()
    ordered_non_na <- sort(unique_densities[!is.na(unique_densities)])
    ordered_densities <- c(NA, ordered_non_na)

    for (dens in ordered_densities) {
        tab_label <- if (is.na(dens)) "Default" else as.character(dens)
        cat("#### ", tab_label, "\n\n", sep = "")

        data_slice <- if (is.na(dens)) {
            aug_results |> filter(is.na(density))
        } else {
            aug_results |> filter(density == dens)
        }

        if (nrow(data_slice) > 0) {
            print(
                aug_factor_impact(
                    data = data_slice,
                    plot_metric = metric
                )
            )
        } else {
            cat("No observations available for this density value.\n\n")
        }

        cat("\n\n")
    }
}

# Create boxplots of performance metrics
# one plot corresponds to one augmentation factor & density of GHL matrix
# each plot is displayed in separate tabset
render_boxplot_per_density <- function(
    aug_results,
    benchmark_results,
    metric,
    aug_factors = NULL,
    sel_density = c(NA_real_, 0.1, 0.5),
    heading_prefix = "k = "
) {
    if (is.null(aug_factors)) {
        aug_factors <- sort(unique(aug_results$augmentation_factor))
    }
    # Filter input data only to considered density
    if (!is.na(sel_density)) {
        aug_results <- aug_results |>
            filter(density == sel_density)
    } else {
        aug_results <- aug_results |>
            filter(is.na(density))
    }
    # Filter benchmark - plot only results for standard ILR transformed data
    benchmark_results <- benchmark_results |>
        filter(transform == "standard_ilr")

    for (k in aug_factors) {
        cat("### ", heading_prefix, k, "\n\n", sep = "")
        print(
            boxplot_perf_metric(
                aug_res = aug_results,
                benchmark_res = benchmark_results,
                plot_metric = metric,
                aug_factor = k
            )
        )
        cat("\n\n")
    }

    #invisible(NULL)
}

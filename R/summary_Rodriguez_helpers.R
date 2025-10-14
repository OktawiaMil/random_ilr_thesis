# Helper functions for Quarto docs summarising the results
# of Rodriguez augmentation methods

# Prepare performance metrics for summary tables:
# - convert accuracy into misclassification_rate (1 - accuracy)
# - keep only the metrics of interest (ROC AUC and misclassification rate)
prepare_summary_metrics <- function(data) {
  if ("transform" %in% names(data) && !"augmentation" %in% names(data)) {
    data <- data |> rename(augmentation = transform)
  }

  preserved_cols <- intersect(
    c("data_id", "augmentation", "augmentation_factor"),
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
    select(-source_metric)
}

# Compute mean performance metric for augmented and benchmark models
# Combine them and create wide summary table
summarise_metric_table <- function(
  benchmark,
  aug_results,
  aug_factor,
  perf_metric,
  digits = 3
) {
  bench_summary <- benchmark |>
    prepare_summary_metrics() |>
    filter(.metric == perf_metric) |>
    group_by(data_id, augmentation) |>
    summarise(mean_value = mean(.estimate), .groups = "drop")

  aug_summary <- aug_results |>
    filter(augmentation_factor == aug_factor) |>
    prepare_summary_metrics() |>
    filter(.metric == perf_metric) |>
    group_by(data_id, augmentation) |>
    summarise(mean_value = mean(.estimate), .groups = "drop")

  # 2 benchmarks only for LASSO
  model <- unique(benchmark$model)
  if (model != "lasso") {
    bench_summary <- bench_summary |>
      filter(augmentation == "proportion")
  }

  combined <- bind_rows(bench_summary, aug_summary) |>
    mutate(mean_value = round(mean_value, digits))

  wide <- combined |>
    pivot_wider(names_from = augmentation, values_from = mean_value)

  known_cols <- c(
    "proportion",
    "standard_ilr",
    "aitchison_mixup",
    "comp_cutmix",
    "comp_feature"
  )
  present_cols <- intersect(known_cols, names(wide))
  select_cols <- c("data_id", present_cols)

  wide |>
    mutate(data_id = as.integer(data_id)) |>
    arrange(data_id) |>
    select(all_of(select_cols)) |>
    mutate(
      augmentation_factor = aug_factor,
      .metric = perf_metric
    ) |>
    relocate(augmentation_factor, .metric, .before = data_id)
}

# Create summary tables for all combinations of perf. metric
# and augmentation factors
build_summary_dataset <- function(benchmark, aug_results, digits = 3) {
  aug_factors <- aug_results |>
    pull(augmentation_factor) |>
    unique() |>
    sort()

  metrics_available <- union(
    prepare_summary_metrics(aug_results) |>
      distinct(.metric) |>
      pull(.metric),
    prepare_summary_metrics(benchmark) |>
      distinct(.metric) |>
      pull(.metric)
  )

  metric_order <- c("roc_auc", "misclassification_rate")
  metrics <- metric_order[metric_order %in% metrics_available]

  expand_grid(
    augmentation_factor = aug_factors,
    .metric = metrics
  ) |>
    mutate(
      data = map2(
        augmentation_factor,
        .metric,
        ~ summarise_metric_table(
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

# Format final summary table
format_summary_table <- function(
  summary_df,
  digits = 3,
  format = c("html", "latex")
) {
  format <- match.arg(format)

  value_cols <- setdiff(
    names(summary_df),
    c("augmentation_factor", ".metric", "data_id")
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
          digits,
          format
        )
      )
    ) |>
    ungroup()
}

# Save selected summary table with mean performance as Latex code
save_metric_table_latex <- function(
  benchmark,
  aug_results,
  aug_factor,
  perf_metric,
  model = c("lasso", "lasso_ilr", "xgboost", "random_forest"),
  table_caption = NULL,
  table_name,
  output_dir,
  digits = 3
) {
  model <- match.arg(model)

  summary_wide <- summarise_metric_table(
    benchmark = benchmark,
    aug_results = aug_results,
    aug_factor = aug_factor,
    perf_metric = perf_metric,
    digits = digits
  )

  formatted <- format_summary_table(
    summary_wide,
    digits = digits,
    format = "latex"
  )

  available_aug <- intersect(
    c(
      "proportion",
      "standard_ilr",
      "aitchison_mixup",
      "comp_cutmix",
      "comp_feature"
    ),
    names(formatted)
  )

  select_cols <- c("data_id", available_aug)
  display_df <- formatted |>
    select(all_of(select_cols)) |>
    mutate(data_id = as.integer(data_id))

  has_standard <- "standard_ilr" %in% available_aug
  has_proportion <- "proportion" %in% available_aug

  rename_old <- select_cols
  rename_new <- c("Dataset")

  for (col in available_aug) {
    label <- dplyr::case_when(
      col == "proportion" && has_standard ~ "Proportion",
      col == "proportion" && !has_standard ~ "Benchmark",
      col == "standard_ilr" ~ "Standard ILR",
      col == "aitchison_mixup" ~ "Aitchison Mixup",
      col == "comp_cutmix" ~ "Comp. Cutmix",
      col == "comp_feature" ~ "Comp. Feature Dropout",
      TRUE ~ col
    )
    rename_new <- c(rename_new, label)
  }

  rename_map <- stats::setNames(rename_old, rename_new)
  display_df <- display_df |>
    rename(!!!rename_map)

  align <- c("l", rep("c", ncol(display_df) - 1))

  table_obj <- kableExtra::kbl(
    display_df,
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = align,
    caption = table_caption,
    label = paste0("tab:", table_name)
  ) |>
    kableExtra::row_spec(0, bold = TRUE)

  has_dual_benchmark <- all(
    c("Proportion", "Standard ILR") %in% names(display_df)
  )

  if (model %in% c("lasso", "lasso_ilr") && has_dual_benchmark) {
    table_obj <- table_obj |>
      kableExtra::add_header_above(
        c(
          " " = 1,
          "Benchmark" = 2,
          "Aitchison Mixup" = 1,
          "Comp. Cutmix" = 1,
          "Comp. Feature Dropout" = 1
        ),
        align = c("c", "c", "c", "c", "c")
      )
  }

  table_obj <- table_obj |>
    kableExtra::kable_styling(latex_options = c("hold_position"))

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file_path <- file.path(output_dir, paste0(table_name, ".tex"))
  kableExtra::save_kable(table_obj, file = file_path)
  invisible(file_path)
}

# Function used to create the interactive table with the mean of
# the selected perfromance metric for chosen augmentation factor
create_summary_dashboard <- function(
  benchmark,
  aug_results,
  digits = 3,
  id_prefix = "summary"
) {
  summary_wide <- build_summary_dataset(
    benchmark = benchmark,
    aug_results = aug_results
  )

  formatted <- format_summary_table(
    summary_wide,
    digits = digits,
    format = "html"
  )

  # Rename columns
  name_map <- c(
    "Augmentation factor" = "augmentation_factor",
    "Metric" = ".metric",
    "Dataset" = "data_id",
    "Proportion" = "proportion",
    "Standard ILR" = "standard_ilr",
    "Aitchison Mixup" = "aitchison_mixup",
    "Comp. Cutmix" = "comp_cutmix",
    "Comp. Feature Dropout" = "comp_feature"
  )

  rename_map <- name_map[name_map %in% names(formatted)]
  formatted <- formatted |>
    rename(!!!rename_map)

  formatted <- formatted |>
    mutate(
      Metric = dplyr::recode(
        Metric,
        roc_auc = "ROC AUC",
        misclassification_rate = "Misclassification Rate",
        .default = Metric
      )
    )

  metric_levels <- c("ROC AUC", "Misclassification Rate")
  metric_levels_present <- metric_levels[
    metric_levels %in% unique(formatted$Metric)
  ]
  if (length(metric_levels_present) == 0) {
    metric_levels_present <- unique(formatted$Metric)
  }

  # Ordering table w.r.t. augmentation factor, metric, dataset
  formatted <- formatted |>
    mutate(
      `Augmentation factor` = factor(
        `Augmentation factor`,
        levels = sort(unique(`Augmentation factor`))
      ),
      Metric = factor(Metric, levels = metric_levels_present),
      Dataset = as.integer(Dataset)
    ) |>
    arrange(`Augmentation factor`, Metric, Dataset)

  # Detect if we have 1 or 2 benchmarks
  has_standard <- "Standard ILR" %in% names(formatted)
  has_proportion <- "Proportion" %in% names(formatted)

  if (!has_standard && has_proportion) {
    formatted <- formatted |>
      rename(Benchmark = `Proportion`)
  }

  bench_cols <- if (has_standard) {
    intersect(c("Proportion", "Standard ILR"), names(formatted))
  } else if ("Benchmark" %in% names(formatted)) {
    "Benchmark"
  } else {
    character()
  }

  # Choose the columns to show in table
  aug_cols <- intersect(
    c("Aitchison Mixup", "Comp. Cutmix", "Comp. Feature Dropout"),
    names(formatted)
  )

  display_cols <- c(
    "Augmentation factor",
    "Metric",
    "Dataset",
    bench_cols,
    aug_cols
  )
  formatted <- formatted |>
    select(all_of(display_cols))

  if (has_standard) {
    # Build a two-row header with Benchmark spanning the two benchmark columns
    other_cols <- setdiff(
      display_cols,
      c("Augmentation factor", "Metric", "Dataset", bench_cols)
    )
    bench_header <- if (length(bench_cols) > 0) {
      list(htmltools::tags$th(colspan = length(bench_cols), "Benchmark"))
    } else {
      list()
    }
    top_cells <- c(
      list(
        htmltools::tags$th(rowspan = 2, "Augmentation factor"),
        htmltools::tags$th(rowspan = 2, "Metric"),
        htmltools::tags$th(rowspan = 2, "Dataset")
      ),
      bench_header,
      lapply(other_cols, function(col) htmltools::tags$th(rowspan = 2, col))
    )

    top_row <- Reduce(
      htmltools::tagAppendChild,
      top_cells,
      init = htmltools::tags$tr()
    )

    head_rows <- list(top_row)
    if (length(bench_cols) > 0) {
      second_cells <- lapply(bench_cols, function(col) htmltools::tags$th(col))
      second_row <- Reduce(
        htmltools::tagAppendChild,
        second_cells,
        init = htmltools::tags$tr()
      )
      head_rows <- c(head_rows, list(second_row))
    }

    table_container <- htmltools::tags$table(
      class = "display",
      do.call(htmltools::tags$thead, head_rows)
    )
  } else {
    # Build a single-row header (1 Benchamrk - no need to have 2 rows of column names)
    header_cells <- lapply(
      names(formatted),
      function(col) htmltools::tags$th(col)
    )
    header_row <- Reduce(
      htmltools::tagAppendChild,
      header_cells,
      init = htmltools::tags$tr()
    )
    table_container <- htmltools::tags$table(
      class = "display",
      htmltools::tags$thead(header_row)
    )
  }

  shared <- SharedData$new(
    formatted,
    key = ~ paste(Metric, `Augmentation factor`, Dataset),
    group = id_prefix
  )

  # 2 drop-down filters: metric, augmentation factor
  controls <- bscols(
    widths = c(6, 6),
    filter_select(
      paste0(id_prefix, "_metric"),
      "Performance metric",
      shared,
      ~Metric
    ),
    filter_select(
      paste0(id_prefix, "_factor"),
      "Augmentation factor",
      shared,
      ~`Augmentation factor`
    )
  )

  datatable_widget <- datatable(
    shared,
    escape = FALSE,
    rownames = FALSE,
    container = table_container,
    options = list(
      dom = "tip",
      pageLength = 12,
      ordering = FALSE,
      autoWidth = TRUE
    )
  )

  browsable(tagList(controls, datatable_widget))
}


generate_metric_latex <- function(
  benchmark,
  aug_results,
  metric,
  model_name,
  output_dir,
  metric_label = metric,
  table_prefix = "rodriguez",
  pseudo_count = NULL,
  caption_fun = NULL
) {
  if (is.null(caption_fun)) {
    caption_fun <- function(
      aug_factor,
      metric_label,
      model_name,
      pseudo_count = NULL
    ) {
      if (!is.null(pseudo_count)) {
        sprintf(
          "Mean %s of %s (augmentation factor = %s, pseudo-count = %s)",
          metric_label,
          model_name,
          aug_factor,
          pseudo_count
        )
      } else {
        sprintf(
          "Mean %s of %s (augmentation factor = %s)",
          metric_label,
          model_name,
          aug_factor
        )
      }
    }
  }

  aug_factors <- sort(unique(aug_results$augmentation_factor))

  caption_inputs <- list(
    aug_factor = NULL,
    metric_label = metric_label,
    model_name = model_name,
    pseudo_count = pseudo_count
  )

  walk(aug_factors, function(aug_factor) {
    caption_inputs$aug_factor <- aug_factor
    formal_names <- names(formals(caption_fun))
    args_to_use <- caption_inputs
    if (!is.null(formal_names) && length(formal_names) > 0) {
      formal_names <- setdiff(formal_names, "...")
      if (length(formal_names) > 0) {
        formal_names <- formal_names[formal_names %in% names(caption_inputs)]
        args_to_use <- args_to_use[formal_names]
      }
    }

    save_metric_table_latex(
      benchmark = benchmark,
      aug_results = aug_results,
      aug_factor = aug_factor,
      perf_metric = metric,
      model = model_name,
      table_caption = do.call(caption_fun, args_to_use),
      table_name = sprintf(
        "%s_%s_mean_%s_%s",
        table_prefix,
        model_name,
        metric,
        aug_factor
      ),
      output_dir = output_dir
    )
  })

  invisible(NULL)
}

# Create boxplots of performance metrics
# one plot corresponds to one augmentation factor
# each plot is displayed in separate tabset
render_metric_panels <- function(
  aug_results,
  benchmark_results,
  metric,
  aug_factors = NULL,
  heading_prefix = "k = "
) {
  if (is.null(aug_factors)) {
    aug_factors <- sort(unique(aug_results$augmentation_factor))
  }

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


render_roc_curve_panels <- function(
  aug_results,
  data_ids = NULL,
  heading_prefix = "k = "
) {
  if (is.null(data_ids)) {
    data_ids <- sort(unique(as.numeric(aug_results$data_id)))
  }

  for (id in data_ids) {
    cat("### ", heading_prefix, id, "\n\n", sep = "")
    print(
      roc_curve_summary(
        aug_res = aug_results,
        sel_id = id
      )
    )
    cat("\n\n")
  }

  #invisible(NULL)
}

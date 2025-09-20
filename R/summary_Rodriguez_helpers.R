# Helper functions for `summary_rodriguez.qmd`

# Determine whether smaller or larger velues of performance metric
# indicate good model's perfromance
metric_direction <- function(metric) {
  metric <- tolower(metric)
  if (metric %in% c("roc_auc", "accuracy")) {
    return("max")
  }
  if (
    metric %in%
      c(
        "brier_class",
        "misclassification_rate"
      )
  ) {
    return("min")
  }
  "max"
}

# Make bolded the best perfromance metric for each row (model) in the table
format_metric_value <- function(
  value,
  row_values,
  metric,
  digits = 3,
  format = c("html", "latex")
) {
  format <- match.arg(format)

  if (is.na(value)) {
    return(NA_character_)
  }

  direction <- metric_direction(metric)
  target <- if (direction == "max") {
    suppressWarnings(max(row_values, na.rm = TRUE))
  } else {
    suppressWarnings(min(row_values, na.rm = TRUE))
  }

  if (!is.finite(target)) {
    target <- NA_real_
  }

  formatted <- sprintf(paste0("%.", digits, "f"), value)
  if (!is.na(target) && value == target) {
    formatted <- if (format == "html") {
      sprintf("<strong>%s</strong>", formatted)
    } else {
      sprintf("\\textbf{%s}", formatted)
    }
  }
  formatted
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
    rename(augmentation = transform) |>
    select(data_id, augmentation, perf_metrics) |>
    unnest(perf_metrics) |>
    filter(.metric == perf_metric) |>
    group_by(data_id, augmentation) |>
    summarise(mean_value = mean(.estimate), .groups = "drop")

  aug_summary <- aug_results |>
    filter(augmentation_factor == aug_factor) |>
    select(data_id, augmentation, perf_metrics) |>
    unnest(perf_metrics) |>
    filter(.metric == perf_metric) |>
    group_by(data_id, augmentation) |>
    summarise(mean_value = mean(.estimate), .groups = "drop")

  combined <- bind_rows(bench_summary, aug_summary) |>
    mutate(mean_value = round(mean_value, digits))

  required_cols <- c(
    "proportion",
    "standard_ilr",
    "aitchison_mixup",
    "comp_cutmix",
    "comp_feature"
  )

  wide <- combined |>
    pivot_wider(names_from = augmentation, values_from = mean_value)

  missing_cols <- setdiff(required_cols, names(wide))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      wide[[col]] <- NA_real_
    }
  }

  wide |>
    mutate(data_id = as.integer(data_id)) |>
    arrange(data_id) |>
    select(
      data_id,
      any_of(required_cols)
    ) |>
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

  metrics_aug <- aug_results |>
    select(perf_metrics) |>
    unnest(perf_metrics) |>
    distinct(.metric) |>
    pull(.metric)

  metrics_bench <- benchmark |>
    select(perf_metrics) |>
    unnest(perf_metrics) |>
    distinct(.metric) |>
    pull(.metric)

  metrics <- union(metrics_aug, metrics_bench)

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
  model = c("lasso", "xgboost", "random_forest"),
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

  display_df <- formatted |>
    select(
      data_id,
      proportion,
      standard_ilr,
      aitchison_mixup,
      comp_cutmix,
      comp_feature
    ) |>
    mutate(data_id = as.integer(data_id)) |>
    rename(
      "Dataset" = data_id,
      "Proportion" = proportion,
      "Standard ILR" = standard_ilr,
      "Aitchison Mixup" = aitchison_mixup,
      "Comp. Cutmix" = comp_cutmix,
      "Comp. Feature Dropout" = comp_feature
    )

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

  if (model == "lasso") {
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

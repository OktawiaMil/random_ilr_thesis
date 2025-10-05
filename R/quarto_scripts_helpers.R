boxplot_perf_metric <- function(
  aug_res,
  benchmark_res,
  aug_factor = 2,
  plot_metric = c(
    "roc_auc",
    "brier_class",
    "accuracy",
    "misclassification_rate"
  )
) {
  model <- unique(aug_res$model)
  plot_metric <- match.arg(plot_metric)

  if (model == "lasso") {
    model <- str_to_upper(model)
  } else if (model == "xgboost") {
    model <- "XGBoost"
  } else {
    model <- model |> str_replace("_", " ") |> str_to_title()
  }

  # Merge benchmark and augmented dataset results
  data_plot <- benchmark_res |>
    rename(augmentation = transform) |>
    select(data_id, model, augmentation, perf_metrics) |>
    mutate(augmentation_factor = "None") |>
    bind_rows(
      aug_res |>
        select(
          data_id,
          model,
          augmentation,
          perf_metrics,
          augmentation_factor
        ) |>
        filter(augmentation_factor == aug_factor) |>
        mutate(augmentation_factor = as.character(augmentation_factor))
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
      mutate(misclas = 1 - .estimate, .metric = "Misclassification Rate") |>
      select(-c(".estimate")) |>
      rename(.estimate = misclas)
  }

  data_plot <- data_plot |>
    mutate(
      augmentation = case_when(
        augmentation == "comp_cutmix" ~ "Compositional CutMix",
        augmentation == "comp_feature" ~ "Compositional Feature Dropout",
        augmentation == "aitchison_mixup" ~ "Aitchison Mixup",
        augmentation == "proportion" ~ "Benchmark - Proportion",
        augmentation == "standard_ilr" ~ "Benchmark - Standard ILR",
        augmentation == "aug_in_n" ~ "randomILR Augmentation in n",
        augmentation == "aug_in_p" ~ "randomILR Augmentation in p"
      ),
      data_id = factor(data_id, levels = as.character(c(1:12))),
      is_benchmark = str_detect("Benchmark", augmentation)
    )

  # make sure that in legend you have first benchmark(s)
  # then augmentation strategies
  lvl_bench <- data_plot |>
    filter(is_benchmark) |>
    pull(augmentation) |>
    unique() |>
    sort()

  lvl_others <- data_plot |>
    filter(!is_benchmark) |>
    pull(augmentation) |>
    unique() |>
    sort()

  data_plot <- data_plot |>
    mutate(
      augmentation = factor(augmentation, levels = c(lvl_bench, lvl_others))
    )

  plot_metric_name <- unique(data_plot$.metric)

  data_plot |>
    ggplot(aes(x = data_id, y = .estimate, fill = augmentation)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw() +
    labs(
      title = paste("Boxplot of", plot_metric_name, "for", model),
      subtitle = paste("Augmentation factor:", aug_factor),
      x = "Dataset",
      y = plot_metric_name
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 90),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
}

roc_curve_summary <- function(aug_res, sel_id) {
  data_plot <- aug_res %>%
    mutate(data_id = as.integer(data_id)) %>%
    filter(data_id == sel_id) %>%
    mutate(
      augmentation = case_when(
        augmentation == "comp_cutmix" ~ "Compositional CutMix",
        augmentation == "comp_feature" ~ "Compositional Feature Dropout",
        augmentation == "aitchison_mixup" ~ "Aitchison Mixup",
        augmentation == "aug_in_n" ~ "randomILR Augmentation in n",
        augmentation == "aug_in_p" ~ "randomILR Augmentation in p"
      ),
      model = case_when(
        model == "xgboost" ~ "XGBoost",
        model == "lasso" ~ "LASSO",
        model == "random_forest" ~ "Random Forest"
      )
    ) %>%
    unnest(roc_curve)

  data_mean <- data_plot %>%
    mutate(x_val = 1 - specificity) %>%
    group_by(augmentation, x_val) %>%
    summarise(
      y_mean = mean(sensitivity),
      y_sd = sd(sensitivity)
    ) %>%
    mutate(
      err_low = y_mean - y_sd,
      err_high = y_mean + y_sd
    ) %>%
    ungroup()

  mean_roc_auc <- data_plot %>%
    unnest(perf_metrics) %>%
    filter(.metric == "roc_auc") %>%
    group_by(augmentation) %>%
    summarise(mean_auc = mean(.estimate))

  lab_df <- data.frame(
    augmentation = mean_roc_auc$augmentation,
    x = -Inf,
    y = Inf,
    label = paste("Mean AUC", round(mean_roc_auc$mean_auc, 2))
  )

  model_name <- unique(data_plot$model)

  ggplot(data_plot, aes(x = 1 - specificity, y = sensitivity, group = split)) +
    geom_line(alpha = 0.5, linetype = 3, colour = "grey40", linewidth = 0.7) +
    geom_line(
      data = data_mean,
      aes(x = x_val, y = y_mean),
      color = "#C24841FF",
      inherit.aes = FALSE,
      linewidth = 1
    ) +
    geom_ribbon(
      data = data_mean,
      aes(x = x_val, ymin = err_low, ymax = err_high),
      fill = "grey70",
      inherit.aes = FALSE,
      alpha = 0.5
    ) +
    facet_grid(~augmentation) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = 2,
      colour = "steelblue",
      linewidth = 1
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = paste(
        model_name,
        "ROC curves for task",
        sel_id,
        "by augmentation strategy"
      ),
      subtitle = "Grey lines: individual ROC curves, red line: mean curve, shaded band: mean ± 1 st. dev."
    ) +
    geom_text(
      data = lab_df,
      mapping = aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = -0.1,
      vjust = 1.3,
      size = 4.5
    ) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 90),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(size = 12)
    )
}

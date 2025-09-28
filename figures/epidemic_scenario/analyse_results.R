source(here::here("packages.R"))

version <- "figure_4_epidemic_simulations"
files <- list.files(
  path = file.path("figures", "epidemic_scenario", version),
  full.names = TRUE
)

get_results <- function(sim) {
  df <- pivot_to_long(sim$simulation)
  results <- list(
    "epidemic_final_size" = epidemic_final_size(df),
    "time_to_peak_infections" = time_to_peak_infections(
      df,
      dt = sim$parameters$dt
    ),
    "peak_daily_incidence" = peak_daily_incidence(df, dt = sim$parameters$dt),
    "id" = sim$parameters$id
  )
  return(results)
}

results <- purrr::map(files, readRDS) |>
  purrr::map_dfr(get_results)

# simulation_settings obtained from previous script. Possible we want to save this with the simulations
results <- arrange(results, id) |>
  left_join(
    simulation_settings,
    by = "id"
  )

results_long <- results |>
  tidyr::pivot_longer(
    cols = c(
      "epidemic_final_size",
      "time_to_peak_infections",
      "peak_daily_incidence"
    ),
    names_to = "metric",
    values_to = "value"
  )

update_labels <- function(df) {
  df |>
    mutate(
      coverage_type = case_when(
        coverage_type == "random" ~ "Random",
        coverage_type == "targeted_riskiness" ~ "Targeted",
      ),
      metric_label = case_when(
        metric == "epidemic_final_size" ~ "Final size",
        metric == "time_to_peak_infections" ~ "Time to peak infections",
        metric == "peak_daily_incidence" ~ "Peak daily incidence"
      ),
      archetype_label = case_when(
        archetype == "flu" ~ "Influenza",
        archetype == "sars_cov_2" ~ "SARS-CoV-2"
      ),
      coverage_label = "Coverage strategy",
      efficacy_label = "Efficacy"
    )
}

results_summary <- results_long |>
  group_by(coverage_type, coverage, archetype, efficacy, metric) |>
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  update_labels()

pointrange_plot_metric <- function(metric, archetype) {
  results_summary |>
    filter(
      metric_label == .env$metric,
      archetype_label == .env$archetype
    ) |>
    ggplot(aes(x = factor(coverage))) +
    geom_pointrange(
      aes(
        y = mean_value,
        ymin = mean_value - sd_value,
        ymax = mean_value + sd_value
      ),
      position = position_dodge(width = 0.1),
    ) +
    ggh4x::facet_nested(
      coverage_label + coverage_type ~ efficacy_label + efficacy
    ) +
    labs(
      title = paste0(archetype, ": ", metric),
      x = "Coverage",
      y = metric
    ) +
    theme_minimal()
}

heatmap_plot_metric <- function(metric, archetype, palette) {
  results_summary |>
    filter(
      metric_label == .env$metric,
      archetype_label == .env$archetype
    ) |>
    ggplot(aes(x = factor(coverage), y = factor(efficacy))) +
    geom_tile(aes(fill = mean_value)) +
    facet_wrap(~coverage_type) +
    labs(
      title = paste0(archetype, ": ", metric),
      x = "Coverage",
      y = "Efficacy",
      fill = metric
    ) +
    ggplot2::scale_fill_viridis_c(option = palette) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
}

# N.B.: Time to peak is questionable as metric: include in appendix?

final_size_flu_pointrange <- pointrange_plot_metric("Final size", "Influenza") +
  scale_y_continuous(labels = scales::percent)

peak_incidence_flu_pointrange <- pointrange_plot_metric(
  "Peak daily incidence",
  "Influenza"
)

final_size_flu_heatmap <- heatmap_plot_metric("Final size", "Influenza", "mako")

peak_incidence_flu_heatmap <- heatmap_plot_metric(
  "Peak daily incidence",
  "Influenza",
  "rocket"
)

flu_plot <- (final_size_flu_pointrange +
  peak_incidence_flu_pointrange +
  final_size_flu_heatmap +
  peak_incidence_flu_heatmap)

ggsave(
  filename = here::here("figures", "epidemic_scenario", "flu.png"),
  plot = flu_plot,
  width = 9,
  height = 8,
  units = "in",
  dpi = 300
)

final_size_covid_pointrange <- pointrange_plot_metric(
  "Final size",
  "SARS-CoV-2"
) +
  scale_y_continuous(labels = scales::percent)

peak_incidence_covid_pointrange <- pointrange_plot_metric(
  "Peak daily incidence",
  "SARS-CoV-2"
)

final_size_covid_heatmap <- heatmap_plot_metric(
  "Final size",
  "SARS-CoV-2",
  "mako"
)

peak_incidence_covid_heatmap <- heatmap_plot_metric(
  "Peak daily incidence",
  "SARS-CoV-2",
  "rocket"
)

covid_plot <- (final_size_covid_pointrange +
  peak_incidence_covid_pointrange +
  final_size_covid_heatmap +
  peak_incidence_covid_heatmap)

ggsave(
  filename = here::here("figures", "epidemic_scenario", "covid.png"),
  plot = covid_plot,
  width = 9,
  height = 8,
  units = "in",
  dpi = 300
)

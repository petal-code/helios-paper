# Add 100% efficacy to the simulations

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
    "peak_daily_incidence" = peak_daily_incidence(
      df,
      dt = sim$parameters$dt,
      per = 1000
    ),
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
        metric == "epidemic_final_size" ~ "Final % of\nPopulation Infected",
        metric == "time_to_peak_infections" ~ "Time to peak infections",
        metric == "peak_daily_incidence" ~
          "Peak Daily Incidence\nper 1,000 Population"
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
    max_value = max(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  update_labels()

pointrange_plot_metric <- function(metric, archetype) {
  df <- results_summary |>
    filter(
      metric == .env$metric,
      archetype_label == .env$archetype,
      efficacy > 0.2
    )

  ggplot(df, aes(x = factor(coverage), col = as.factor(efficacy))) +
    geom_line(aes(y = mean_value, group = as.factor(efficacy))) +
    geom_pointrange(
      aes(
        y = mean_value,
        ymin = min_value,
        ymax = max_value
      ),
      position = position_dodge(width = 0.1),
    ) +
    ggh4x::facet_nested(
      . ~ coverage_type
    ) +
    labs(
      x = "Coverage",
      col = "Efficacy",
      y = df$metric_label[1]
    ) +
    theme_minimal() +
    theme(
      legend.position = "left"
    )
}

heatmap_plot_metric <- function(metric, archetype) {
  df <- results_summary |>
    filter(
      metric == .env$metric,
      archetype_label == .env$archetype
    )

  ggplot(df, aes(x = factor(coverage), y = factor(efficacy))) +
    geom_tile(aes(fill = mean_value)) +
    ggh4x::facet_nested(
      . ~ coverage_type
    ) +
    labs(
      x = "Coverage",
      y = "Efficacy",
      fill = df$metric_label[1]
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
}

# N.B.: Time to peak is questionable as metric: include in appendix?

covid_col1 <- c("#5083DB", "#395D9C", "#253B65")
covid_col2 <- c("#83d8b4", "#3BB585", "#1d5d43")
flu_col1 <- c("#CD86EA", "#651983", "#3F1052")
flu_col2 <- c("#E68996", "#D93052", "#9D374C")

final_size_flu_pointrange <- pointrange_plot_metric(
  "epidemic_final_size",
  "Influenza"
) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = flu_col1)

peak_incidence_flu_pointrange <- pointrange_plot_metric(
  "peak_daily_incidence",
  "Influenza"
) +
  scale_colour_manual(values = flu_col2)

final_size_flu_heatmap <- heatmap_plot_metric(
  "epidemic_final_size",
  "Influenza"
) +
  ggplot2::scale_fill_viridis_c(option = "magma", labels = scales::percent)

peak_incidence_flu_heatmap <- heatmap_plot_metric(
  "peak_daily_incidence",
  "Influenza"
) +
  ggplot2::scale_fill_viridis_c(option = "magma")

final_size_covid_pointrange <- pointrange_plot_metric(
  "epidemic_final_size",
  "SARS-CoV-2"
) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = covid_col1)

peak_incidence_covid_pointrange <- pointrange_plot_metric(
  "peak_daily_incidence",
  "SARS-CoV-2"
) +
  scale_colour_manual(values = covid_col2)

final_size_covid_heatmap <- heatmap_plot_metric(
  "epidemic_final_size",
  "SARS-CoV-2"
) +
  ggplot2::scale_fill_viridis_c(option = "mako", labels = scales::percent)

peak_incidence_covid_heatmap <- heatmap_plot_metric(
  "peak_daily_incidence",
  "SARS-CoV-2"
) +
  ggplot2::scale_fill_viridis_c(option = "mako")

row1 <- final_size_covid_pointrange + final_size_covid_heatmap
row2 <- peak_incidence_covid_pointrange + peak_incidence_covid_heatmap
row3 <- final_size_flu_pointrange + final_size_flu_heatmap
row4 <- peak_incidence_flu_pointrange + peak_incidence_flu_heatmap

plot <- row1 / row2 / row3 / row4

ggsave(
  filename = here::here("figures", "epidemic_scenario", "figure4.png"),
  plot = plot,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

readr::write_csv(
  results_summary,
  here::here("figures", "epidemic_scenario", "results_summary.csv")
)

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
      )
    )
}

ggplot(results_long, aes(x = id, y = value)) +
  geom_point() +
  facet_wrap(~metric, scales = "free") +
  theme_minimal()

results_summary <- results_long |>
  group_by(coverage_type, coverage, efficacy, metric) |>
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  update_labels()

ggplot(results_summary, aes(x = coverage, y = efficacy)) +
  geom_tile(aes(fill = mean_value)) +
  facet_wrap(. ~ metric_label, scales = "free") +
  theme_minimal()

plot_metric <- function(metric) {
  results_summary |>
    mutate(
      coverage_label = "Coverage strategy",
      efficacy_label = "Efficacy"
    ) |>
    filter(metric_label == .env$metric) |>
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
      title = metric,
      x = "Coverage",
      y = metric
    ) +
    theme_minimal()
}

final_size_pointrange <- plot_metric("Final size") +
  scale_y_continuous(labels = scales::percent)

ggsave(
  filename = file.path(
    "figures",
    "epidemic_scenario",
    "final_size_pointrange.png"
  ),
  plot = final_size_pointrange,
  width = 7,
  height = 5,
  bg = "white"
)

peak_incidence_pointrange <- plot_metric("Peak daily incidence")

ggsave(
  filename = file.path(
    "figures",
    "epidemic_scenario",
    "peak_incidence_pointrange.png"
  ),
  plot = peak_incidence_pointrange,
  width = 7,
  height = 5,
  bg = "white"
)

time_to_peak_pointrange <- plot_metric("Time to peak infections")

ggsave(
  filename = file.path(
    "figures",
    "epidemic_scenario",
    "time_to_peak_pointrange.png"
  ),
  plot = time_to_peak_pointrange,
  width = 7,
  height = 5,
  bg = "white"
)

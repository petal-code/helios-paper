source(here::here("packages.R"))

version <- "figure_4_epidemic_simulations"
files <- list.files(path = file.path("figures", "epidemic_scenario", version), full.names = TRUE)

get_results <- function(sim) {
  df <- pivot_to_long(sim$simulation)
  results <- list(
    "epidemic_final_size" = epidemic_final_size(df),
    "time_to_peak_infections" = time_to_peak_infections(df, dt = sim$parameters$dt),
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
    cols = c("epidemic_final_size", "time_to_peak_infections", "peak_daily_incidence"),
    names_to = "metric",
    values_to = "value"
  )

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
  )

ggplot(results_summary, aes(x = coverage, y = efficacy)) +
  geom_tile(aes(fill = mean_value)) +
  facet_wrap(. ~ metric, scales = "free") +
  theme_minimal()

results_summary |>
  filter(metric == "epidemic_final_size") |>
  ggplot(aes(x = coverage)) +
  geom_pointrange(
    aes(y = mean_value, ymin = mean_value - sd_value, ymax = mean_value + sd_value),
    position = position_dodge(width = 0.1)
  ) +
  facet_grid(coverage_type ~ efficacy) +
  labs(title = "Final size")

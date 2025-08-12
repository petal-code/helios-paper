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

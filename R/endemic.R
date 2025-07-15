annual_incidence <- function(
  df,
  start_time = min(df$timestep),
  end_time = max(df$timestep),
  timesteps_per_year = 365
) {
  df |>
    filter(State == "Infected") |>
    arrange(timestep) |>
    mutate(incidence = Proportion - lag(Proportion, default = 0)) |>
    filter(
      timestep >= start_time,
      timestep <= end_time
    ) |>
    summarise(
      incidence = sum(pmax(incidence, 0), na.rm = TRUE),
      time = (end_time - start_time + 1) / timesteps_per_year,
      rate = incidence / time
    ) |>
    pull(rate)
}

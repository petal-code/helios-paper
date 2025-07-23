annual_incidence <- function(
  df,
  start_timestep = min(df$timestep),
  end_timestep = max(df$timestep),
  dt = 1
) {
  df |>
    filter(state == "I") |>
    arrange(timestep) |>
    mutate(incidence = proportion - lag(proportion, default = 0)) |>
    filter(
      timestep >= start_time,
      timestep <= end_time
    ) |>
    summarise(
      incidence = sum(pmax(incidence, 0), na.rm = TRUE),
      time = (end_timestep - start_timestep + 1) * dt / 365,
      rate = incidence / time
    ) |>
    pull(rate)
}

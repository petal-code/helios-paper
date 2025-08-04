time_to_peak_infections <- function(df, dt) {
  df |>
    filter(state == "I") |>
    slice_max(order_by = proportion, n = 1, with_ties = FALSE) |>
    mutate(time_to_peak = dt * timestep) |>
    pull(time_to_peak)
}

epidemic_final_size <- function(df) {
  df |>
    filter(state %in% c("E", "I", "R")) |>
    slice_max(order_by = timestep, n = 1, with_ties = TRUE) |>
    summarise(final_size = sum(proportion)) |>
    pull(final_size)
}

peak_daily_incidence <- function(df, dt) {
  df |>
    filter(state == "S") |>
    arrange(timestep) |>
    mutate(
      incidence = lag(proportion, default = 0) - proportion,
      day = floor((timestep - min(timestep)) * dt)
    ) |>
    group_by(day) |>
    summarise(
      daily_incidence = sum(incidence, na.rm = TRUE),
      .groups = "drop"
    ) |>
    summarise(peak = max(daily_incidence, na.rm = TRUE)) |>
    pull(peak)
}

time_to_peak <- function(df) {
  df |>
    filter(State == "Infected") |>
    slice_max(order_by = Proportion, n = 1, with_ties = FALSE) |>
    rename(time_to_peak = timestep) |>
    pull(time_to_peak)
}

epidemic_final_size <- function(df) {
  df |>
    filter(State %in% c("Exposed", "Infected", "Recovered")) |>
    slice_max(order_by = timestep, n = 1, with_ties = TRUE) |>
    summarise(final_size = sum(Proportion)) |>
    pull(final_size)
}

peak_incidence <- function(df) {
  df |>
    filter(State == "Infected") |>
    arrange(timestep) |>
    mutate(
      incidence = Proportion - lag(Proportion, default = 0)
    ) |>
    summarise(peak_incidence = max(incidence, na.rm = TRUE)) |>
    pull(peak_incidence)
}

time_to_peak <- function(df) {
  id_cols <- c("Setting", "Intervention")
  df |>
    filter(State == "Infected") |>
    group_by(across(all_of(id_cols))) |>
    slice_max(order_by = Proportion, n = 1, with_ties = FALSE) |>
    select(timestep, all_of(id_cols)) |>
    rename(time_to_peak = timestep)
}

epidemic_final_size <- function(df) {
  id_cols <- c("Setting", "Intervention")
  df |>
    filter(State == "Recovered") |>
    group_by(across(all_of(id_cols))) |>
    slice_max(order_by = timestep, n = 1, with_ties = FALSE) |>
    select(Proportion, all_of(id_cols)) |>
    rename(final_size = Proportion)
}

peak_incidence <- function(df) {
  id_cols <- c("Setting", "Intervention")
  df |>
    filter(State == "Infected") |>
    group_by(across(all_of(id_cols))) |>
    arrange(timestep) |>
    mutate(incidence = Proportion - lag(Proportion, default = 0)) |>
    summarise(peak_incidence = max(incidence, na.rm = TRUE), .groups = "drop")
}

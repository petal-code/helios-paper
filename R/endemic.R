annual_incidence <- function(
  df,
  start_timestep = min(df$timestep),
  end_timestep = max(df$timestep),
  dt = 1,
  type = "proportion",
  per = 1
) {
  type <- rlang::arg_match(type, c("proportion", "count"))

  rate <- df |>
    filter(state == "S") |>
    arrange(timestep) |>
    mutate(incidence = lag(.data[[type]], default = 0) - .data[[type]]) |>
    filter(
      timestep >= start_timestep,
      timestep <= end_timestep
    ) |>
    summarise(
      incidence = sum(pmax(incidence, 0), na.rm = TRUE),
      time = (end_timestep - start_timestep + 1) * dt / 365,
      rate = incidence / time
    ) |>
    pull(rate)

  # For proportions we optionally multiply by per to give a rate for a given population size
  if (type == "proportion") {
    rate <- rate * per
  }

  return(rate)
}

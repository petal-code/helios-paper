annual_incidence <- function(
  df,
  start_timestep = min(df$timestep),
  end_timestep = max(df$timestep),
  dt = 1,
  type = "proportion",
  per = NULL
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

  if (type == "proportion" && !is.null(per)) {
    rate <- rate * per
  } else if (type == "count" && !is.null(per)) {
    population <- df |>
      filter(timestep == max(timestep)) |>
      summarise(population = sum(count)) |>
      pull(population)

    rate <- rate * per / population
  }

  return(rate)
}

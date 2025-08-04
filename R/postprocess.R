pivot_to_long <- function(df) {
  df |>
    select(-E_new) |>
    tidyr::pivot_longer(
      cols = ends_with("_count"),
      names_to = "state",
      values_to = "count",
      names_pattern = "(.*)_"
    ) |>
    group_by(timestep) |>
    mutate(
      proportion = count / sum(count)
    ) |>
    ungroup()
}

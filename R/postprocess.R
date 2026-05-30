pivot_to_long <- function(df) {
  if (all(c("I_mild_count", "I_hosp_count") %in% names(df))) {
    df <- df |>
      mutate(I_count = I_mild_count + I_hosp_count) |>
      select(-I_mild_count, -I_hosp_count)
  }

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

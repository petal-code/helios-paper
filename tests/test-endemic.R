sims <- readRDS("example_simulations.rds")

id_cols <- c("Setting", "Intervention")

sims |>
  group_by(across(all_of(id_cols))) |>
  group_modify(
    ~ tibble(
      annual_incidence = annual_incidence(.x),
    )
  )

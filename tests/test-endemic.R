source(here::here("packages.R"))

sims <- readRDS(here::here("tests", "example_simulations.rds"))

sims <- sims |>
  rename(
    proportion = Proportion,
    state = State
  ) |>
  mutate(
    state = forcats::fct_recode(
      state,
      "S" = "Susceptible",
      "E" = "Exposed",
      "I" = "Infected",
      "R" = "Recovered",
    )
  )

id_cols <- c("Setting", "Intervention")

sims |>
  group_by(across(all_of(id_cols))) |>
  group_modify(
    ~ tibble(
      annual_incidence = annual_incidence(.x),
    )
  )

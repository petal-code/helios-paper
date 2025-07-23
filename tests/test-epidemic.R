source(here::here("packages.R"))

download.file(
  url = "https://raw.githubusercontent.com/mrc-ide/helios/main/vignettes/Blueprint_Report2_July24_files/exemplar_simulations.rds",
  destfile = "example_simulations.rds",
  mode = "wb" # important for binary files
)

sims <- readRDS("example_simulations.rds")

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
      time_to_peak_infections = time_to_peak_infections(.x),
      final_size = epidemic_final_size(.x),
      peak_incidence = peak_incidence(.x)
    )
  )

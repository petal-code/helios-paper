source(here::here("packages.R"))

# Shared parameters between scenarios
config <- list(
  years_to_simulate = 1,
  timestep_uvc_on = 1,
  human_population = 10000,
  setting_size = list(
    size_per_individual_workplace = 10,
    size_per_individual_school = 3.33,
    size_per_individual_leisure = 2,
    size_per_individual_household = 20
  )
)

parameter_lists <- tidyr::crossing(
  archetype = c("flu"),
  coverage_type = c("random", "targeted_riskiness"),
  coverage = 0.5,
  riskiness = "setting_specific_riskiness",
  efficacy = 0.8,
  iteration = 1:5,
  scenario = "epidemic",
) |>
  mutate(id = row_number()) |>
  purrr::pmap(expand_parameters)


parameter_lists

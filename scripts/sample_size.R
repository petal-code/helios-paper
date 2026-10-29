source(here::here("packages.R"))

config <- list(
  dt = 0.5,
  simulation_time_days = 4 * 365,
  timestep_uvc_on = 1,
  disable_hospitalisation = TRUE,
  human_population = 50000,
  setting_size = list(
    size_per_individual_workplace = 10,
    size_per_individual_school = 3.33,
    size_per_individual_leisure = 2,
    size_per_individual_household = 20
  )
)

# Number of simulations per scenario
N <- 100

# Representative intervention: random allocation, 40% coverage, 60% efficacy
# Coverage zero gives the corresponding scenario without intervention

simulation_settings <- tidyr::crossing(
  archetype = c("flu", "sars_cov_2"),
  coverage_type = "random",
  coverage = c(0, 0.4),
  riskiness = "setting_specific_riskiness",
  efficacy = 0.6,
  figure = 4,
  iteration = 1:N,
  scenario = "epidemic"
) |>
  mutate(
    id = row_number(),
    seed = 100000L + id
  )

parameter_lists <- purrr::pmap(
  simulation_settings,
  expand_parameters,
  config = config
)

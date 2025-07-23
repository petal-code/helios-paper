source(here::here("packages.R"))

# Shared parameters between scenarios
config <- list(
  dt = 0.5,
  simulation_time_days = 4 * 365,
  timestep_uvc_on = 1,
  human_population = 50000,
  setting_size = list(
    size_per_individual_workplace = 10,
    size_per_individual_school = 3.33,
    size_per_individual_leisure = 2,
    size_per_individual_household = 20
  )
)

# Generate all parameter lists for this figure
parameter_lists <- tidyr::crossing(
  archetype = c("flu", "sars_cov_2"),
  coverage_type = c("random", "targeted_riskiness"),
  coverage = seq(0.2, 0.8, by = 0.2),
  riskiness = "setting_specific_riskiness",
  efficacy = seq(0.2, 0.8, by = 0.2),
  iteration = 1:3,
  scenario = "epidemic",
) |>
  mutate(id = row_number()) |>
  purrr::pmap(expand_parameters, config = config)

# Total number of simulations to run
length(parameter_lists)

tictoc::tic()
x <- helios::run_simulation(parameter_lists[[1]])
tictoc::toc()

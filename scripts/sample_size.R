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

slug <- ids::adjective_animal()
time <- format(Sys.time(), "%Y%m%d_%H%M%S")
name1 <- paste("simulation_settings", slug, time, sep = "-")
name2 <- paste("parameter_lists", slug, time, sep = "-")

data_dir <- here::here("scripts", "sample_size")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(
  simulation_settings,
  file.path(data_dir, paste0(name1, ".rds"))
)

saveRDS(
  parameter_lists,
  file.path(data_dir, paste0(name2, ".rds"))
)

# Run all parameter lists and save each result
output_dir <- file.path(data_dir, paste("outputs", slug, time, sep = "-"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

purrr::walk(
  parameter_lists,
  \(parameters) run_simulation_hipercow(
    parameters,
    file_save = TRUE,
    directory = paste0(output_dir, "/")
  ),
  .progress = "Running simulations"
)

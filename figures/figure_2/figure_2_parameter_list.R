source(here::here("packages.R"))

#Core Parameters
archetypes <- c("flu", "sars_cov_2")
iterations <- 1:15
years_to_simulate <- 15
simulation_time_days <- (365 * years_to_simulate)
human_population <- 100000
duration_of_immunity <- 365
external_infection_probability <- 1 / human_population
riskiness <- "setting_specific_riskiness"
dt <- 0.5
size_per_individual_workplace <- 10
size_per_individual_school <- 3.33
size_per_individual_leisure <- 2
size_per_individual_household <- 20

# Initial conditions for SARS-CoV-2:
initial_S_SC2 <- round(0.4 * human_population)
initial_E_SC2 <- round(0.01 * human_population)
initial_I_SC2 <- round(0.02 * human_population)
initial_R_SC2 <- human_population -
  initial_S_SC2 -
  initial_E_SC2 -
  initial_I_SC2

# Initial conditions for Flu:
initial_S_flu <- round(0.67 * human_population)
initial_E_flu <- round(0.006 * human_population)
initial_I_flu <- round(0.012 * human_population)
initial_R_flu <- human_population -
  initial_S_flu -
  initial_E_flu -
  initial_I_flu

simulations_to_run <- rbind(
  expand.grid(
    archetype = archetypes,
    coverage = seq(0.2, 1.0, 0.2),
    efficacy = seq(0.2, 1.0, 0.2),
    coverage_type = "random",
    iteration = iterations,
    riskiness = riskiness,
    stringsAsFactors = FALSE
  )
)

simulations_to_run <- simulations_to_run |>
  mutate(
    scenario = "endemic",
    ID = 1:n(),
    seed = 1000 + ID
  ) |>
  arrange(
    archetype,
    coverage_type,
    coverage,
    efficacy,
    iteration
  )

parameter_lists <- list()

for (i in 1:nrow(simulations_to_run)) {
  if (simulations_to_run$archetype[i] == "sars_cov_2") {
    parameter_lists[[i]] <- get_parameters(
      archetype = simulations_to_run$archetype[i],
      overrides = list(
        human_population = human_population,
        number_initial_S = initial_S_SC2,
        number_initial_E = initial_E_SC2,
        number_initial_I = initial_I_SC2,
        number_initial_R = initial_R_SC2,
        endemic_or_epidemic = "endemic",
        dt = dt,
        size_per_individual_workplace = size_per_individual_workplace,
        size_per_individual_school = size_per_individual_school,
        size_per_individual_leisure = size_per_individual_leisure,
        size_per_individual_household = size_per_individual_household,
        duration_immune = duration_of_immunity,
        prob_inf_external = external_infection_probability,
        simulation_time = simulation_time_days,
        seed = simulations_to_run$seed[i]
      )
    )
  } else if (simulations_to_run$archetype[i] == "flu") {
    parameter_lists[[i]] <- get_parameters(
      archetype = simulations_to_run$archetype[i],
      overrides = list(
        human_population = human_population,
        number_initial_S = initial_S_flu,
        number_initial_E = initial_E_flu,
        number_initial_I = initial_I_flu,
        number_initial_R = initial_R_flu,
        endemic_or_epidemic = "endemic",
        dt = dt,
        size_per_individual_workplace = size_per_individual_workplace,
        size_per_individual_school = size_per_individual_school,
        size_per_individual_leisure = size_per_individual_leisure,
        size_per_individual_household = size_per_individual_household,
        duration_immune = duration_of_immunity,
        prob_inf_external = external_infection_probability,
        simulation_time = simulation_time_days,
        seed = simulations_to_run$seed[i]
      )
    )
  }

  # UVC Parameters
  if (simulations_to_run$coverage[i] > 0) {
    parameter_lists[[i]] <- parameter_lists[[i]] %>%
      set_uvc(
        setting = "joint",
        coverage = simulations_to_run$coverage[i],
        coverage_target = "square_footage",
        coverage_type = simulations_to_run$coverage_type[i],
        efficacy = simulations_to_run$efficacy[i],
        timestep = 365 * 10 * 2
      )
  }

  #Riskiness Settings
  if (simulations_to_run$riskiness[i] == "setting_specific_riskiness") {
    parameter_lists[[i]] <- parameter_lists[[i]] %>%
      set_setting_specific_riskiness(
        setting = "school",
        mean = 0,
        sd = 0.3544,
        min = 1 / sqrt(4.75),
        max = sqrt(4.75)
      ) %>%
      set_setting_specific_riskiness(
        setting = "workplace",
        mean = 0,
        sd = 0.5072,
        min = 1 / sqrt(6.35),
        max = sqrt(6.35)
      ) %>%
      set_setting_specific_riskiness(
        setting = "household",
        mean = 0,
        sd = 0.0871,
        min = 1 / sqrt(2.5),
        max = sqrt(2.5)
      ) %>%
      set_setting_specific_riskiness(
        setting = "leisure",
        mean = 0,
        sd = 0.4278,
        min = 1 / sqrt(5.5),
        max = sqrt(5.5)
      )
  }
}

for (i in 1:length(parameter_lists)) {
  parameter_lists[[i]]$simulation_id <- simulations_to_run$ID[i]
  parameter_lists[[i]]$iteration_number <- simulations_to_run$iteration[i]
  parameter_lists[[i]]$archetype_label <- simulations_to_run$archetype[i]
  parameter_lists[[i]]$coverage <- simulations_to_run$coverage[i]
  parameter_lists[[i]]$efficacy <- simulations_to_run$efficacy[i]
  parameter_lists[[i]]$coverage_type <- simulations_to_run$coverage_type[i]
}

saveRDS(parameter_lists, "figures/figure_2/figure_2_parameter_list.rds")
#saveRDS(
#  simulations_to_run,
#  "figures/figure_2/figure_2_parameter_combinations.rds"
#)

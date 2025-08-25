source(here::here("packages.R"))

#Core Parameters
iterations <- 1:5
years_to_simulate <- 20
simulation_time_days <- (365 * years_to_simulate)
human_population <- 50000
duration_of_immunity <- 365
external_infection_probability <- 1 / human_population
archetypes <- c("sars_cov_2", "flu")
iterations <- 1:10
riskiness <- "setting_specific_riskiness"

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

# Figure 3: Random vs Targeted UVC Installation Comparison

simulations_to_run_fig3 <- rbind(
  # Panel A: Effects of Efficacy of Benefits of Targeting
  # Line Plot,x-axis = efficacy, y = % reduction in annualized disease incidence

  expand.grid(
    archetype = c("sars_cov_2", "flu"),
    coverage = 0.5,
    efficacy = c(0, 0.2, 0.4, 0.6, 0.8),
    coverage_type = c("random", "targeted_riskiness"),
    iteration = iterations,
    panel = "panel_3A",
    riskiness = "setting_specific_riskiness",
    stringsAsFactors = FALSE
  ),

  # Panel B: Compares random vs targeted at various coverage levels

  expand.grid(
    archetype = c("sars_cov_2", "flu"),
    coverage = seq(0.1, 0.9, 0.1),
    efficacy = 0.5,
    coverage_type = c("random", "targeted_riskiness"),
    iteration = iterations,
    panel = "panel_B",
    riskiness = "setting_specific_riskiness",
    stringsAsFactors = FALSE
  ),

  # Panel C: Bar Plot comparing low targeted vs high random coverage
  expand.grid(
    archetype = c("sars_cov_2", "flu"),
    coverage = c(0.3, 0.4, 0.5, 0.6, 0.7),
    efficacy = c(0.3, 0.5, 0.7),
    coverage_type = c("random", "targeted_riskiness"),
    iteration = iterations,
    panel = "panel_C",
    riskiness = "setting_specific_riskiness",
    stringsAsFactors = FALSE
  ),

  # Panel D: Heat map comparing the difference between random and targeted across coverage and efficacy
  # Plots the additional % reduction from targeting (targeted_reduction - random_reduction)
  expand.grid(
    archetype = c("sars_cov_2", "flu"),
    coverage = c(0.2, 0.4, 0.6, 0.8),
    efficacy = c(0.2, 0.4, 0.6, 0.8),
    coverage_type = c("random", "targeted_riskiness"),
    iteration = iterations,
    panel = "panel_D",
    riskiness = "setting_specific_riskiness",
    stringsAsFactors = FALSE
  )
)

simulations_to_run_fig3 <- simulations_to_run_fig3 %>%
  mutate(scenario = "endemic") %>%
  arrange(archetype, panel, coverage_type, coverage, efficacy, iteration) %>%
  mutate(ID = 1:nrow(simulations_to_run_fig3)) %>%
  mutate(seed = 1000 + ID)

parameter_lists_fig3 <- list()

for (i in 1:nrow(simulations_to_run_fig3)) {
  # Base parameters by archetype
  if (simulations_to_run_fig3$archetype[i] == "sars_cov_2") {
    parameter_lists_fig3[[i]] <- get_parameters(
      archetype = simulations_to_run_fig3$archetype[i],
      overrides = list(
        human_population = human_population,
        number_initial_S = initial_S_SC2,
        number_initial_E = initial_E_SC2,
        number_initial_I = initial_I_SC2,
        number_initial_R = initial_R_SC2,
        endemic_or_epidemic = "endemic",
        duration_immune = duration_of_immunity,
        prob_inf_external = external_infection_probability,
        simulation_time = simulation_time_days,
        seed = simulations_to_run_fig3$seed[i]
      )
    )
  } else if (simulations_to_run_fig3$archetype[i] == "flu") {
    parameter_lists_fig3[[i]] <- get_parameters(
      archetype = simulations_to_run_fig3$archetype[i],
      overrides = list(
        human_population = human_population,
        number_initial_S = initial_S_flu,
        number_initial_E = initial_E_flu,
        number_initial_I = initial_I_flu,
        number_initial_R = initial_R_flu,
        endemic_or_epidemic = "endemic",
        duration_immune = duration_of_immunity,
        prob_inf_external = external_infection_probability,
        simulation_time = simulation_time_days,
        seed = simulations_to_run_fig3$seed[i]
      )
    )
  }

  if (simulations_to_run_fig3$coverage[i] > 0) {
    parameter_lists_fig3[[i]] <- parameter_lists_fig3[[i]] %>%
      set_uvc(
        setting = "joint",
        coverage = simulations_to_run_fig3$coverage[i],
        coverage_target = "square_footage",
        coverage_type = simulations_to_run_fig3$coverage_type[i],
        efficacy = simulations_to_run_fig3$efficacy[i],
        timestep = 0
      )
  }

  parameter_lists_fig3[[i]] <- parameter_lists_fig3[[i]] %>%
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

  parameter_lists_fig3[[i]]$simulation_id <- simulations_to_run_fig3$ID[i]
  parameter_lists_fig3[[
    i
  ]]$iteration_number <- simulations_to_run_fig3$iteration[i]
  parameter_lists_fig3[[i]]$panel <- simulations_to_run_fig3$panel[i]
  parameter_lists_fig3[[i]]$coverage <- simulations_to_run_fig3$coverage[i]
  parameter_lists_fig3[[i]]$efficacy <- simulations_to_run_fig3$efficacy[i]
  parameter_lists_fig3[[
    i
  ]]$coverage_type <- simulations_to_run_fig3$coverage_type[i]
}

saveRDS(parameter_lists_fig3, "figure_3_parameter_list.rds")
saveRDS(simulations_to_run_fig3, "figure_3_parameter_combinations.rds")

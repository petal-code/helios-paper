source(here::here("packages.R"))
#Figure 3: Targeted vs Random
# Core Parameters
archetypes <- c("flu", "sars_cov_2")
iterations <- 1:5
years_to_simulate <- 20
simulation_time_days <- (365 * years_to_simulate)
human_population <- 50000
duration_of_immunity <- 365
external_infection_probability <- 1 / human_population
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

simulations_to_run <- rbind(
  # Panel A: Absolute reduction in annualized incidence vs baseline
  # % reduction plotted for random vs targeted
  # Panel E: Absolute reduction in prevalence vs baseline (reuses these runs)
  expand.grid(
    archetype     = archetypes,
    coverage      = seq(0.1, 0.9, 0.1),   # fine coverage sweep
    efficacy      = c(0.3, 0.5, 0.8),     # Low / Medium / High efficacy
    coverage_type = c("random", "targeted_riskiness"),
    iteration     = iterations,
    panel         = "panel_A",
    riskiness     = riskiness,
    stringsAsFactors = FALSE
  ),
  
  # Panel B: Relative extra impact in annualized incidence
  # Panel F: Relative extra impact in prevalence (reuses these runs)
  expand.grid(
    archetype     = archetypes,
    coverage      = seq(0.1, 0.9, 0.1),
    efficacy      = c(0.3, 0.5, 0.8),
    coverage_type = c("random", "targeted_riskiness"),
    iteration     = iterations,
    panel         = "panel_B",
    riskiness     = riskiness,
    stringsAsFactors = FALSE
  ),
  
  # Panel C: Heatmap of absolute impact (incidence, targeted − random)
  expand.grid(
    archetype     = archetypes,
    coverage      = c(0.2, 0.4, 0.6, 0.8),
    efficacy      = c(0.2, 0.4, 0.6, 0.8),
    coverage_type = c("random", "targeted_riskiness"),
    iteration     = iterations,
    panel         = "panel_C",
    riskiness     = riskiness,
    stringsAsFactors = FALSE
  ),
  
  # Panel D: Heat map of relative extra impact (incidence)
  #  same coverage × efficacy grid as Panel C
  expand.grid(
    archetype     = archetypes,
    coverage      = c(0.2, 0.4, 0.6, 0.8),
    efficacy      = c(0.2, 0.4, 0.6, 0.8),
    coverage_type = c("random", "targeted_riskiness"),
    iteration     = iterations,
    panel         = "panel_D",
    riskiness     = riskiness,
    stringsAsFactors = FALSE
  )
)

simulations_to_run <- simulations_to_run |>
  dplyr::mutate(
    scenario = "endemic",
    ID = 1:n(),
    seed = 1000 + ID
  ) |>
  dplyr::arrange(
    archetype,
    panel,
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
        timestep = 365*15
      )
  }

  # Riskiness Settings
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

simulations_to_run <- simulations_to_run |>
  dplyr::mutate(
    figure = 3,
  ) |>
  select(
    ID,
    figure,
    scenario,
    iteration,
    panel,
    everything()
  )

for (i in 1:length(parameter_lists)) {
  parameter_lists[[i]]$figure <- simulations_to_run$figure[i]
  parameter_lists[[i]]$scenario <- simulations_to_run$scenario[i]
  parameter_lists[[i]]$id <- simulations_to_run$ID[i]
  parameter_lists[[i]]$iteration <- simulations_to_run$iteration[i]
  parameter_lists[[i]]$panel <- simulations_to_run$panel[i]
  parameter_lists[[i]]$archetype_label <- simulations_to_run$archetype[i]
  parameter_lists[[i]]$coverage <- simulations_to_run$coverage[i]
  parameter_lists[[i]]$efficacy <- simulations_to_run$efficacy[i]
  parameter_lists[[i]]$coverage_type <- simulations_to_run$coverage_type[i]
}

saveRDS(parameter_lists, "figures/figure_3/figure_3_parameter_list.rds")
saveRDS(
  simulations_to_run,
  "figures/figure_3/figure_3_parameter_combinations.rds"
)

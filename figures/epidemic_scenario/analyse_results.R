source(here::here("packages.R"))

version <- "figure_4_epidemic_simulations"
files <- list.files(path = file.path("figures", "epidemic_scenario", version), full.names = TRUE)

# archetype
# coverage_type
# coverage
# riskiness
# efficacy
# iteration
# scenario
# id
# seed

sim <- readRDS(files[1])

df <- pivot_to_long(sim$simulation)
epidemic_final_size <- epidemic_final_size(df)
time_to_peak_infections <- time_to_peak_infections(df, dt = sim$parameters$dt)
peak_daily_incidence <- peak_daily_incidence(df, dt = sim$parameters$dt)

source(here::here("packages.R"))

params <- get_parameters(archetype = "flu")
x <- run_simulation(params)

pivot_to_long(x)

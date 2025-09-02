source(here::here("packages.R"))

params <- get_parameters(archetype = "flu")
x <- run_simulation(params)

df <- pivot_to_long(x)

annual_incidence(df, type = "count")
annual_incidence(df, type = "proportion")

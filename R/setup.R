init_counts <- function(N, prop_E = 0.005) {
  E <- floor(N * prop_E)
  if (E == 0) {
    cli::cli_warn(
      "Initialising with no exposed individuals. This is unlikely to be desirable"
    )
  }
  S <- N - E
  list(
    number_initial_S = S,
    number_initial_E = E,
    number_initial_I = 0,
    number_initial_R = N - S - E
  )
}

# Takes the basic parameters then builds out the complete parameter list
expand_parameters <- function(
  archetype,
  coverage_type,
  coverage,
  riskiness,
  efficacy,
  iteration,
  figure,
  scenario,
  id,
  seed,
  config
) {
  overrides <- rlang::inject(list(
    dt = config$dt,
    human_population = config$human_population,
    simulation_time = config$simulation_time_days,
    !!!(config$setting_size),
    !!!init_counts(config$human_population)
  ))

  param <- get_parameters(
    archetype = archetype,
    overrides = overrides
  )

  if (coverage > 0) {
    param <- set_uvc(
      param,
      setting = "joint",
      coverage = coverage,
      coverage_target = "square_footage",
      coverage_type = coverage_type,
      efficacy = efficacy,
      timestep = config$timestep_uvc_on
    )
  }

  if (riskiness == "setting_specific_riskiness") {
    param <- set_riskiness(param)
  }

  param$figure <- figure
  param$scenario <- scenario
  param$iteration <- iteration
  param$id <- id
  param$seed <- seed
  return(param)
}

# Could put these in config
set_riskiness <- function(df) {
  df |>
    set_setting_specific_riskiness(
      setting = "school",
      mean = 0,
      sd = 0.3544,
      min = 1 / sqrt(4.75),
      max = sqrt(4.75)
    ) |>
    set_setting_specific_riskiness(
      setting = "workplace",
      mean = 0,
      sd = 0.5072,
      min = 1 / sqrt(6.35),
      max = sqrt(6.35)
    ) |>
    set_setting_specific_riskiness(
      setting = "household",
      mean = 0,
      sd = 0.0871,
      min = 1 / sqrt(2.5),
      max = sqrt(2.5)
    ) |>
    set_setting_specific_riskiness(
      setting = "leisure",
      mean = 0,
      sd = 0.4278,
      min = 1 / sqrt(5.5),
      max = sqrt(5.5)
    )
}

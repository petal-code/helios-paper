library(parallel)
library(individual)
library(helios)

parameter_lists <- readRDS("figure_2_parameter_list.rds")

run_parallel_simulations <- function(
  parameter_lists,
  cores = detectCores() - 1
) {
  cl <- makeCluster(cores)
  clusterExport(cl, varlist = c("parameter_lists"), envir = environment())
  clusterEvalQ(cl, {
    library(individual)
    library(helios)
  })
  run_single_simulation <- function(i) {
    parameters <- parameter_lists[[i]]
    result <- run_simulation(parameters)
    #Annualized Disease Incidence
    total_new_infections <- sum(result$E_new, na.rm = TRUE)
    observation_period_years <- (nrow(result) * parameters$dt) / 365
    annualized_incidence <- total_new_infections / (parameters$human_population * observation_period_years)
    mean_incidence <- mean(result$I_count) / parameters$human_population
    
    #Active Infection Prevalence
    active_infections <- result$E_count + result$I_count
    mean_prevalence <- mean(active_infections)
    return(list(
      sim_id = i,
      simulation_id = parameters$simulation_id,
      archetype = parameters$archetype,
      coverage = parameters$coverage,
      efficacy = parameters$efficacy,
      coverage_type = parameters$coverage_type,
      iteration = parameters$iteration_number,
      annualized_incidence = annualized_incidence,
      mean_prevalence = mean_prevalence,
      full_results = result
    ))
  }
  
  simulation_results <- parLapply(cl, 1:length(parameter_lists), run_single_simulation)
  stopCluster(cl)
  
  return(simulation_results)
}

results <- run_parallel_simulations(parameter_lists)

results_df <- do.call(rbind, lapply(results, function(x) {
  data.frame(
    sim_id = x$sim_id,
    simulation_id = x$simulation_id,
    archetype = x$archetype,
    coverage = x$coverage,
    efficacy = x$efficacy,
    coverage_type = x$coverage_type,
    iteration = x$iteration,
    annualized_incidence = x$annualized_incidence,
    mean_prevalence = x$mean_prevalence,
    stringsAsFactors = FALSE
  )
}))


saveRDS(results_df, "endemic_simulation_summary.rds")
saveRDS(results, "endemic_simulation_full_results.rds")

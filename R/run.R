run_simulation_hipercow <- function(
    parameters,
    file_save = FALSE,
    directory = NULL
) {
  # Terminate operation if no directory provided:
  if (is.null(directory)) {
    stop("Must provide a directory for output storage")
  }
  
  # Run the simulation:
  s <- helios::run_simulation(parameters_list = parameters)
  
  # Append the simulation identifier to the simulation outputs:
  s$id <- parameters$id
  s$iteration <- parameters$iteration
  s$scenario <- parameters$scenario
  
  # Store parameter list and simulated outputs in a single list for returning:
  output <- list()
  output$parameters <- parameters
  output$simulation <- s
  
  # Get the date/time stamp:
  time_stamp <- format(Sys.time(), "%Y%m%d_%H%M")
  # TODO: Generalise the saveRDS call for use across figures
  # Save the outputs in the specified directory:
  if (file_save) {
    saveRDS(
      object = output,
      file = paste0(
        directory,
        "simulation_id_",
        parameters$id,
        "_iteration_",
        parameters$iteration,
        "_scenario_",
        parameters$scenario,
        "_",
        time_stamp,
        "_output.rds"
      )
    )
  }
  
  return(output)
}

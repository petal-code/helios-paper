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
  s$ID <- parameters$simulation_id
  s$iteration <- parameters$iteration_number
  s$panel <- parameters$panel
  s$archetype <- parameters$archetype
  s$coverage_type <- parameters$coverage_type
  s$coverage <- parameters$coverage
  s$efficacy <- parameters$efficacy
  s$disease_status <- parameters$endemic_or_epidemic
  # Store parameter list and simulated outputs in a single list for returning:
  output <- list()
  output$parameters <- parameters
  output$simulation <- s
  # TODO: Decide on how we want to save these files and amend this:
  # Save the outputs in the specified directory:
  if (file_save) {
    saveRDS(
      object = output,
      file = paste0(
        directory,
        "Simulation_ID_",
        parameters$simulation_id,
        "_iteration_",
        parameters$iteration_number,
        "_",
        parameters$panel,
        "_output.rds"
      )
    )
  }
  return(output)
}
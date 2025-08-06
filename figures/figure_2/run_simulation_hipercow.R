run_simulation_hipercow <- function(parameters, file_save = FALSE, directory = NULL) {
  
  # Load in the helios package:
  #library(helios)
  
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
  
  # Create an output list:
  output <- list()
  
  # Append the simulation output and the parameter list to the output:
  output$parameters <- parameters
  output$simulation <- s
  
  # TODO: Decide on how we want to save these files and amend this:
  # Save the output as a .rds if file_save switched on:
  if(file_save) {
    saveRDS(object = output,
            file = paste0(directory,
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
  
  # Return the outputs
  return(output)
  
}
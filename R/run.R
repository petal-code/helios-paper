assign_simulations <- function(
    n_simulations, 
    n_nodes, 
    distribute_evenly = TRUE
) {
  
  # Ensure inputs are positive
  if(n_nodes <= 0 | n_simulations <= 0) {
    stop(
      "n_nodes and n_simulations must be positive"
    )
  }
  # Ensure inputs are positive
  if(length(n_nodes) != 1 | length(n_simulations) != 1) {
    stop(
      "n_nodes and n_simulations must of length 1"
    )
  }
  # Ensure inputs are positive
  if(n_nodes %% 1 != 0 | n_simulations %% 1 != 0) {
    stop(
      "n_nodes and n_simulations must be integer values"
    )
  }
  
  # Determine number of simulations per node
  base <- floor(n_simulations / n_nodes)
  remainder <- n_simulations %% n_nodes
  
  # Create vector containing the number of simulations to be run on each of the n_nodes. If
  # distribute_evenly = FALSE, all remaining simulations will be added to the final node (which
  # I think should keep their numerical/id order intact). If distribute_evenly = TRUE, the remainder
  # are divided across the n_nodes as evenly as possible.
  sims_per_node <- rep(base, n_nodes)
  if(remainder > 0) {
    if(distribute_evenly == FALSE) {
      sims_per_node[n_nodes] <- sims_per_node[n_nodes] + remainder
    } else if(distribute_evenly == TRUE) {
      sims_per_node[1:remainder] <- sims_per_node[1:remainder] + 1
    }
  } 
  
  # Expand the sims_per_node into a vector containing, for each simulation, the node it will be
  # run on:
  simulation_group_index <- rep(1:n_nodes, times = sims_per_node)
}

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
  s$figure <- parameters$figure
  s$scenario <- parameters$scenario
  s$id <- parameters$id
  s$iteration <- parameters$iteration

  # Store parameter list and simulated outputs in a single list for returning:
  output <- list()
  output$parameters <- parameters
  output$simulation <- s

  # Get the date/time stamp:
  time_stamp <- format(Sys.time(), "%Y%m%d_%H%M")

  # Save the outputs in the specified directory:
  if (file_save) {
    saveRDS(
      object = output,
      file = paste0(
        directory,
        "figure_",
        parameters$figure,
        "_scenario_",
        parameters$scenario,
        "_id_",
        parameters$id,
        "_iteration_",
        parameters$iteration,
        "_scenario_",
        time_stamp,
        "_output.rds"
      )
    )
  }

  return(output)
}

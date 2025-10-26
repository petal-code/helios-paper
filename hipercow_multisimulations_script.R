#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Figure X Hipercow Multisimulation Script  +++++#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++#

##'
##' #+++++ README +++++#
##' #++++++++++++++++++#
##' 
##' This script provides a demonstration, with explanation, of how to run helios-paper simulations
##' on the Imperial cluster using the hipercow package. The script would be expected to be located 
##' in the figures/figure_x directory and run from there.
##' 
##' I've tried to make it figure agnostic by writing things with respect to figure_X - these will
##' need to be modified for the given figure. I have tried to make it such that this should all run
##' through without much further modification, but some further tinkering may be needed.
##' 
##' The script assumes that the current working directory is the repo root (~/helios-paper) - have
##' used the here function to try and generalise this, but a first check for most issues is that
##' your working directory is the root.
##' 
##' The ouputs from the simulations should save into a directory: "figures/figure_X/figure_X_simulations/"
##' which is specifed in the run_simulation_hipercow() call and will need to be made prior to running
##' the simulations (I think - it may generate automatically but not worth chancing it)
##' 

# Set the working directory to the project root:
setwd(dir = here::here())

# Load in the requisite packages:
source(here::here("packages.R"))
source(here::here("R/run.R"))

# Load in the parameter lists - it is anticipated that these 
parameter_lists <- readRDS("figures/figure_X/figure_X_parameter_list.rds")

##' Note: We need to divide the parameter_lists into smaller lists that we can send to each node to
##' be simulated. We do this by specifying the number of nodes to use (n_nodes) and the number of
##' simulations (length(paramter_lists)). The assign_simulations function will return a vector of
##' length(parameter_lists) containing the node on which each simulation will be run. This is then
##' used by the split() function to divide the parameter_lists object into n_nodes lists to be
##' submitted to the cluster for simulating. Setting disrtibute_evenly to FALSE will put any remainder
##' of simulations/nodes onto the final node while setting to TRUE will distribute remainder simulations
##' evenly across the all nodes.

# Determine the number of simulations to run per node: 
nodes_to_use <- 20
group_index <- assign_simulations(
  n_simulations = length(parameter_lists), 
  n_nodes = nodes_to_use, 
  distribute_evenly = FALSE)

# Split the parameter lists into groups to run on individual nodes using the group_indexes calculated
# using the assign_simulations() function.
sub_parameter_lists <- split(
  x = parameter_lists,
  f = group_index
)

#++++++++++++ BODGE SOME LIST SPLITTING ++++++++++++++++++++++++++++++++++#

# Prepare for cluster use (see https://mrc-ide.github.io/hipercow/)
hipercow::hipercow_init(driver = 'dide-windows')
hipercow::hipercow_configure(driver = "dide-windows")

# Provision packages required on the cluster (hipercow looks for provision.R by default)
# see https://mrc-ide.github.io/hipercow/articles/packages.html
hipercow::hipercow_provision()

# Create the environment for hipercow
hipercow::hipercow_environment_create(
  packages = c(
    "individual",
    "helios",
    "tidyverse",
    "dqrng",
    "parallel",
    "EnvStats"
  ),
  sources = "./R/run.R"
)

# TODO: "Better again, create large objects from your 'sources' argument to your environment, and
#        then advertise this using the 'globals' argument (see the hipercow::environments vignette)"
# NOTE: If simulations fail, it is likely that it is due to the memory allocated to each node. A common
#        solution for me has been to increase this using the function below):
# Increase the memory allowed for the parameter lists:
options(hipercow.max_size_local = 10000000)

# Run the simulations using the hipercow function task_create_expr()
# https://mrc-ide.github.io/hipercow/reference/task_create_expr.html
task_ids <- list()
for (i in 1:length(sub_parameter_lists)) {
  task_ids[[i]] <- hipercow::task_create_expr(
    expr = parallel::clusterApply(
      NULL,
      sub_parameter_lists[[i]],
      function(p) {
        run_simulation_hipercow(
          p,
          file_save = TRUE,
          directory = "figures/figure_X/figure_X_simulations/"
        )
      }
    ),
    parallel = hipercow::hipercow_parallel("parallel"),
    resources = hipercow::hipercow_resources(cores = 32)
  )
}

# NOTE: Once we've submitted the simulations we can track their progress using the below. It is often
#       useful to save the task_ids object as this is needed to track them. Have provided the code to
#       save/load them.
# Track the status of the submitted task(s):
x <- sapply(
  task_ids,
  hipercow::task_status
)
table(x)

# Save/load the task_id as required:
#saveRDS(object = task_ids, file = "figures/figure_X/simulation_task_ids.rds")
#task_ids <- readRDS(file = "./figures/figure_X/simulation_task_ids.rds")

# View the job logs:
hipercow::task_log_show(task_ids[[1]])

# View the job result:
#outputs <- hipercow::task_result(task_id)

#--------------------------------------------------------------------------------------------------#

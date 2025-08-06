#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Figure 2 Hipercow Script  +++++#
#+++++++++++++++++++++++++++++++++++++#

##'
##' In this demo scipt we:
##' 1. Configure hipercow
##' 2. Set up some simulations to run in parallel
##' 3. Run the simulations on the DIDE HPC using hipercow
##'

#----- 1) Preamble ---------------------------------------------------------------------------------

# Load in the requisite packages:
library(hipercow)
library(tidyverse)
library(individual)
library(parallel)

# Load in the figure 2 parameter lists to run simulations for:
parameter_lists <- readRDS("figures/figure_2/figure2_parameter_list.rds")

#TODO: Remove:
for(i in 1:length(parameter_lists)) {
  parameter_lists[[i]]$simulation_time <- 5
}
parameter_lists <- parameter_lists[1:64]

#----- 2) hipercow Set-up --------------------------------------------------------------------------

## Prepare for cluster use
## see https://mrc-ide.github.io/hipercow/
hipercow::hipercow_init(driver = 'dide-windows')

# Configure hipercow for dide-windows:
hipercow_configure(driver = "dide-windows")

# Check the configuration:
hipercow::hipercow_configuration()

## Provision packages required on the cluster (hipercow looks for provision.R by default)
## see https://mrc-ide.github.io/hipercow/articles/packages.html
hipercow::hipercow_provision()

#----- 3) Run simulations on cluster using hipercow ------------------------------------------------

# Create the environment for hipercow
hipercow::hipercow_environment_create(packages = c("individual",
                                                   "helios",
                                                   "tidyverse",
                                                   "dqrng",
                                                   "parallel",
                                                   "EnvStats"),
                                      sources = "./figures/figure_2/run_simulation_hipercow.R")

# Run the simulations using the hipercow function task_create_expr()
# https://mrc-ide.github.io/hipercow/reference/task_create_expr.html
task_id <- hipercow::task_create_expr(
  expr = parallel::clusterApply(
    NULL,
    parameter_lists,
    function(p) run_simulation_hipercow(p, file_save = TRUE, directory = "figures/figure_2/figure_2_simulations/")
  ),
  parallel = hipercow::hipercow_parallel("parallel"),
  resources = hipercow::hipercow_resources(cores = 32)
)

# Use this to track the status of your job(s):
x <- sapply(task_id, hipercow::task_status); table(x)

# Save the task_id:
saveRDS(object = task_id, file = "./figures/figure_2/simulation_task_id.rds")
task_id <- readRDS(file = "./figures/figure_2/simulation_task_id.rds")

# View the job logs:
hipercow::task_log_show(task_id)

# View the job result:
#outputs <- hipercow::task_result(task_id)

#--------------------------------------------------------------------------------------------------#
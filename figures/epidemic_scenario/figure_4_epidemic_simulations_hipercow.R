#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Figure 4 Hipercow Script +++++#
#++++++++++++++++++++++++++++++++++++#

source(here::here("packages.R"))

parameter_lists <- readRDS("figures/epidemic_scenario/figure_4_parameter_lists.rds")

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
# then advertise this using the 'globals' argument (see the hipercow::environments vignette)"
# Increase the memory allowed for the parameter lists:
options(hipercow.max_size_local = 5000000)

# Launch a job to run simulations on the cluster via hipercow:
task_id <- hipercow::task_create_expr(
  expr = parallel::clusterApply(
    NULL,
    parameter_lists,
    function(p) {
      run_simulation_hipercow(
        p,
        file_save = TRUE,
        directory = "figures/epidemic_scenario/figure_4_epidemic_simulations/"
      )
    }
  ),
  parallel = hipercow::hipercow_parallel("parallel"),
  resources = hipercow::hipercow_resources(cores = 32)
)

# Track the status of the submitted task(s):
table(
  sapply(
    task_id,
    hipercow::task_status
  )
)

# Save/load the task_id as required:
#saveRDS(object = task_id, file = "./figures/epidemic_scenario/simulation_task_id.rds")
#task_id <- readRDS(file = "./figures/epidemic_scenario/simulation_task_id.rds")

# View the job logs:
hipercow::task_log_show(task_id)

# View the job result:
#outputs <- hipercow::task_result(task_id)

#--------------------------------------------------------------------------------------------------#

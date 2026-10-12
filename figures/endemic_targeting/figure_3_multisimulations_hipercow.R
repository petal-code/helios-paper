#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#+++++ Figure 3 Hipercow Script  +++++#
#+++++++++++++++++++++++++++++++++++++#

source(here::here("packages.R"))

parameter_lists <- readRDS("figures/figure_3/figure_3_parameter_list.rds")

# Split job across 20 32-core nodes:
group_index <- c(rep(1:20, each = 86))
sub_parameter_lists <- split(
  x = parameter_lists,
  f = group_index
)

#++++++++++++ BODGE SOME LIST SPLITTING ++++++++++++++++++++++++++++++++++#

# Prepare for cluster use (see https://mrc-ide.github.io/hipercow/)
hipercow::hipercow_init(driver = 'dide-windows')
hipercow::hipercow_configure(driver = "dide-windows")

## Provision packages required on the cluster (hipercow looks for provision.R by default)
## see https://mrc-ide.github.io/hipercow/articles/packages.html
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
options(hipercow.max_size_local = 26000000)

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
          directory = "figures/figure_3/figure_3_simulations/"
        )
      }
    ),
    parallel = hipercow::hipercow_parallel("parallel"),
    resources = hipercow::hipercow_resources(cores = 32)
  )
}

# Track the status of the submitted task(s):
x <- sapply(
  task_ids,
  hipercow::task_status
)
table(x)

# Save/load the task_id as required:
saveRDS(object = task_ids, file = "figures/figure_3/simulation_task_ids.rds")
task_ids <- readRDS(file = "./figures/figure_3/simulation_task_ids.rds")

# View the job logs:
hipercow::task_log_show(task_ids[[1]])

# View the job result:
#outputs <- hipercow::task_result(task_id)

#--------------------------------------------------------------------------------------------------#

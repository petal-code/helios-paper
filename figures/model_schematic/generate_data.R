source(here::here("packages.R"))

options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(helios)
library(individual)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(tictoc)
theme_set(theme_minimal())

parameters_list <- get_parameters(list(
  human_population = 50000,
  number_initial_S = 40000,
  number_initial_E = 5000,
  number_initial_I = 4000,
  number_initial_R = 1000,
  simulation_time = 100,
  seed = 1
))

names(parameters_list)

variables_list <- create_variables(parameters_list)
variables_list <- variables_list$variables_list
names(variables_list)

map(variables_list, class)

(disease_states <- variables_list$disease_state$get_categories())

parameters_list$number_initial_E
parameters_list$number_initial_I

disease_state_counts <- purrr::map_vec(
  disease_states,
  function(x) variables_list$disease_state$get_size_of(values = x)
)

data.frame("State" = disease_states, "Count" = disease_state_counts) |>
  gt::gt()

parameters_list[c(
  "initial_proportion_child",
  "initial_proportion_adult",
  "initial_proportion_elderly"
)]

age_classes <- variables_list$age_class$get_categories()
age_class_counts <- purrr::map_vec(
  age_classes,
  function(x) variables_list$age_class$get_size_of(values = x)
)

data.frame(age_classes, age_class_counts) |>
  mutate(
    age_classes = forcats::fct_relevel(age_classes, "child", "adult", "elderly")
  ) |>
  ggplot(aes(x = age_classes, y = age_class_counts)) +
  geom_col() +
  labs(x = "Age class", y = "Count") +
  coord_flip()

schools <- variables_list$school$get_categories()
schools <- schools[schools != "0"]
school_sizes <- purrr::map_vec(
  schools,
  function(x) variables_list$school$get_size_of(values = x)
)

data.frame(school_sizes) |>
  ggplot(aes(x = school_sizes)) +
  geom_histogram() +
  labs(x = "School size", y = "Count")

workplaces <- variables_list$workplace$get_categories()
workplaces <- workplaces[workplaces != "0"]
workplace_sizes <- purrr::map_vec(
  workplaces,
  function(x) variables_list$workplace$get_size_of(values = x)
)

data.frame(workplace_sizes) |>
  ggplot(aes(x = log(workplace_sizes))) +
  geom_histogram() +
  labs(x = "log(Workplace size)", y = "Count")

households <- variables_list$household$get_categories()
household_sizes <- purrr::map_vec(
  households,
  function(x) variables_list$household$get_size_of(values = x)
)

table(household_sizes) |>
  data.frame() |>
  ggplot(aes(x = household_sizes, y = Freq)) +
  geom_col() +
  labs(x = "Household size", y = "Count")

household_df <- purrr::map_df(households, function(x) {
  indices <- variables_list$household$get_index_of(values = x)$to_vector()
  if (length(indices > 0))
    data.frame("individual" = indices, "household" = as.numeric(x))
})

age_classes <- variables_list$age_class$get_categories()
age_df <- purrr::map_df(age_classes, function(x) {
  indices <- variables_list$age_class$get_index_of(values = x)$to_vector()
  if (length(indices > 0)) data.frame("individual" = indices, "age_class" = x)
})

household_df |>
  left_join(age_df, by = "individual") |>
  group_by(household) |>
  summarise(
    child = sum(age_class == "child"),
    adult = sum(age_class == "adult"),
    elderly = sum(age_class == "elderly")
  ) |>
  group_by(child, adult, elderly) |>
  summarise(
    count = n()
  ) |>
  ungroup() |>
  arrange(desc(count)) |>
  head() |>
  gt::gt()

leisure_places <- variables_list$leisure$get_values()
number_leisure_places <- sapply(leisure_places, function(x) sum(x > 0))

table(number_leisure_places) |>
  data.frame() |>
  ggplot(aes(x = number_leisure_places, y = Freq)) +
  geom_col() +
  labs(x = "Number of leisure places attended in a week", y = "Count")

events_list <- create_events(
  variables_list = variables_list,
  parameters_list = parameters_list
)
names(events_list)

lapply(events_list, class)

timesteps <- round(parameters_list$simulation_time / parameters_list$dt)

renderer <- individual::Render$new(timesteps)
class(renderer)

variables_list <- create_variables(parameters_list)
parameters_list <- variables_list$parameters_list
variables_list <- variables_list$variables_list
events_list <- create_events(
  variables_list = variables_list,
  parameters_list = parameters_list
)
timesteps <- round(parameters_list$simulation_time / parameters_list$dt)
renderer <- individual::Render$new(timesteps)

processes_list <- create_processes(
  variables_list = variables_list,
  events_list = events_list,
  parameters_list = parameters_list,
  renderer = renderer
)

names(processes_list)

individual::simulation_loop(
  variables = variables_list,
  events = unlist(events_list),
  processes = processes_list,
  timesteps = timesteps,
)

states <- renderer$to_dataframe()

states |>
  tidyr::pivot_longer(
    cols = ends_with("count"),
    names_to = "compartment",
    values_to = "value",
    names_pattern = "(.*)_count"
  ) |>
  mutate(
    compartment = forcats::fct_relevel(compartment, "S", "E", "I", "R")
  ) |>
  ggplot(aes(x = timestep, y = value, col = compartment)) +
  geom_line() +
  scale_color_manual(
    values = c("royalblue3", "firebrick3", "darkorchid3", "orange2")
  ) +
  labs(x = "Time-step", y = "Count", col = "Compartment")

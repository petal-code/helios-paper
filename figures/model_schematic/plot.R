source(here::here("packages.R"))

theme_set(theme_minimal())

parameters_list <- get_parameters(list(
  human_population = 200000,
  number_initial_S = 190000,
  number_initial_E = 5000,
  number_initial_I = 4000,
  number_initial_R = 1000,
  simulation_time = 100,
  seed = 1
))

variables_list_full <- create_variables(parameters_list)
variables_list <- variables_list_full$variables_list
parameters_list <- variables_list_full$parameters_list
disease_states <- variables_list$disease_state$get_categories()

age_classes <- variables_list$age_class$get_categories()

age_class_counts <- purrr::map_vec(
  age_classes,
  function(x) variables_list$age_class$get_size_of(values = x)
)

plot_age <- data.frame(age_classes, age_class_counts) |>
  mutate(
    age_classes = stringr::str_to_title(age_classes),
    age_classes = forcats::fct_relevel(
      age_classes,
      "Child",
      "Adult",
      "Elderly"
    ),
    prop = age_class_counts / sum(age_class_counts),
  ) |>
  ggplot(aes(x = age_classes, y = prop)) +
  geom_col(col = "black", fill = "white") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Age class", y = "")

plot_schools <- data.frame(
  "school_sizes" = parameters_list$setting_sizes$school
) |>
  ggplot(aes(x = school_sizes)) +
  geom_histogram(
    aes(y = stat(count / sum(count))),
    col = "black",
    fill = "white"
  ) +
  scale_x_log10() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "School size (log scale)", y = "")

plot_workplaces <- data.frame(
  "workplace_sizes" = parameters_list$setting_sizes$workplace
) |>
  ggplot(aes(x = workplace_sizes)) +
  geom_histogram(
    aes(y = stat(count / sum(count))),
    col = "black",
    fill = "white"
  ) +
  scale_x_log10() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Workplace size (log scale)", y = "")

households <- variables_list$household$get_categories()

household_sizes <- purrr::map_vec(
  households,
  function(x) variables_list$household$get_size_of(values = x)
)

plot_households <- table(
  "household_sizes" = parameters_list$setting_sizes$household
) |>
  data.frame() |>
  mutate(prop = Freq / sum(Freq)) |>
  ggplot(aes(x = household_sizes, y = prop)) +
  geom_col(col = "black", fill = "white") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Household size", y = "")

household_df <- purrr::map_df(households, function(x) {
  indices <- variables_list$household$get_index_of(values = x)$to_vector()
  if (length(indices > 0)) {
    data.frame("individual" = indices, "household" = as.numeric(x))
  }
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
  gt::gt()

leisure_places <- variables_list$leisure$get_values()

number_leisure_places <- sapply(leisure_places, function(x) sum(x > 0))

plot_leisure_visits <- table(number_leisure_places) |>
  data.frame() |>
  ggplot(aes(x = number_leisure_places, y = Freq)) +
  geom_col(col = "black", fill = "white") +
  labs(x = "Leisure venues attended per week", y = "Count")

plot_leisure <- data.frame(
  "leisure_sizes" = parameters_list$setting_sizes$leisure
) |>
  ggplot(aes(x = leisure_sizes)) +
  geom_histogram(
    aes(y = stat(count / sum(count))),
    col = "black",
    fill = "white"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Leisure venue size", y = "")

timesteps <- round(parameters_list$simulation_time / parameters_list$dt)

renderer <- individual::Render$new(timesteps)

parameters_list <- variables_list_full$parameters_list

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

individual::simulation_loop(
  variables = variables_list,
  events = unlist(events_list),
  processes = processes_list,
  timesteps = timesteps,
)

states <- renderer$to_dataframe()

plot_epidemic <- states |>
  tidyr::pivot_longer(
    cols = ends_with("count"),
    names_to = "compartment",
    values_to = "value",
    names_pattern = "(.*)_count"
  ) |>
  mutate(
    compartment = forcats::fct_relevel(compartment, "S", "E", "I", "R"),
    prop = value / parameters_list$human_population
  ) |>
  ggplot(aes(x = timestep, y = prop, col = compartment)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(
    values = c("royalblue3", "firebrick3", "darkorchid3", "orange2")
  ) +
  labs(x = "Time-step", y = "", col = "")

design <- "
  12
  34
  56
"

plot_age +
  plot_households +
  plot_schools +
  plot_workplaces +
  plot_leisure +
  plot_leisure_visits +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = design, tag_level = "keep")

ggsave("plot.pdf", h = 5.5, w = 7)

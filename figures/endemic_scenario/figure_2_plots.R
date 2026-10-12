source(here::here("packages.R"))

dir_sim <- here::here("figures", "endemic_scenario", "2026-06-04_tom_figure_2")

files <- list.files(
  path = dir_sim,
  pattern = "\\.rds$",
  full.names = TRUE
)

read_simulation <- function(filepath) {
  simulation_file <- readRDS(filepath)
  dt <- simulation_file$parameters$dt
  pop_size <- 50000

  simulation_file$simulation$result |>
    mutate(
      days = timestep * dt,
      year = floor(days / 365),
      filename = basename(filepath),
      archetype = simulation_file$parameters$archetype_label,
      coverage = round(simulation_file$parameters$coverage, 10),
      efficacy = round(simulation_file$parameters$efficacy, 10),
      active_infected = E_count + I_mild_count + I_hosp_count,
      prevalence = (active_infected / pop_size) * 100
    ) |>
    group_by(year, filename, archetype, coverage, efficacy) |>
    summarise(
      total_infections = sum(E_new, na.rm = TRUE),
      annualized_incidence_rate = total_infections / pop_size,
      mean_active_infected = mean(active_infected, na.rm = TRUE),
      mean_prevalence = mean(prevalence, na.rm = TRUE),
      .groups = "drop"
    )
}

all_sims <- purrr::map_dfr(files, read_simulation)

metrics <- all_sims |>
  mutate(
    window = case_when(
      year %in% 5:7 ~ "baseline",
      year %in% 15:17 ~ "post"
    )
  ) |>
  filter(!is.na(window)) |>
  group_by(filename, archetype, coverage, efficacy, window) |>
  summarise(
    mean_incidence_rate = mean(annualized_incidence_rate, na.rm = TRUE),
    mean_active_infected = mean(mean_active_infected, na.rm = TRUE),
    mean_prevalence = mean(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  )

reductions <- metrics |>
  pivot_wider(
    names_from = window,
    values_from = c(mean_incidence_rate, mean_active_infected, mean_prevalence)
  ) |>
  mutate(
    incidence_reduction = 1 -
      mean_incidence_rate_post / mean_incidence_rate_baseline,
    active_infected_reduction = 1 -
      mean_active_infected_post / mean_active_infected_baseline,
    prevalence_reduction = 1 - mean_prevalence_post / mean_prevalence_baseline
  )

reductions_summary <- reductions |>
  group_by(archetype, coverage, efficacy) |>
  summarise(
    mean_incidence_reduction = mean(incidence_reduction, na.rm = TRUE),
    low_incidence = min(incidence_reduction, na.rm = TRUE),
    hi_incidence = max(incidence_reduction, na.rm = TRUE),
    mean_active_reduction = mean(active_infected_reduction, na.rm = TRUE),
    low_active = min(active_infected_reduction, na.rm = TRUE),
    hi_active = max(active_infected_reduction, na.rm = TRUE),
    mean_prevalence_reduction = mean(prevalence_reduction, na.rm = TRUE),
    low_prev = min(prevalence_reduction, na.rm = TRUE),
    hi_prev = max(prevalence_reduction, na.rm = TRUE),
    .groups = "drop"
  )

x_percent_scale <- scale_x_continuous(
  labels = scales::percent_format(accuracy = 1),
  limits = c(0.2, 1.0),
  breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)
)
target_coverage_intervals <- c(0.2, 0.4, 0.6, 0.8, 1.0)
target_efficacies <- c(0.4, 0.6, 0.8)
target_vals <- c(0.2, 0.4, 0.6, 0.8, 1.0)

# Shared presentation settings
figure_theme <- theme_minimal() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.position = "left"
  )

# Panel A & B (SC2 lines)
cols_a <- c("#5083DB", "#395D9C", "#253B65")
plot_data_a <- reductions_summary |>
  filter(
    efficacy %in% target_efficacies,
    archetype == "sars_cov_2",
    coverage %in% target_coverage_intervals
  )

panelA <- ggplot(
  plot_data_a,
  aes(
    x = coverage,
    y = mean_incidence_reduction,
    color = factor(
      efficacy,
      levels = target_efficacies,
      labels = scales::percent(target_efficacies, accuracy = 1)
    ),
    group = efficacy
  )
) +
  geom_line() +
  geom_pointrange(
    aes(ymin = low_incidence, ymax = hi_incidence)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  x_percent_scale +
  scale_color_manual(values = cols_a) +
  figure_theme +
  labs(
    x = "Coverage",
    y = "Annual incidence\nreduction (%)",
    colour = "Efficacy"
  )

cols_b <- cols_a
post_active_sc2 <- metrics |>
  filter(
    window == "post",
    archetype == "sars_cov_2",
    efficacy %in% target_efficacies,
    coverage %in% target_coverage_intervals
  ) |>
  group_by(archetype, coverage, efficacy) |>
  summarise(
    mean_active_infected = mean(mean_prevalence, na.rm = TRUE),
    low = min(mean_prevalence, na.rm = TRUE),
    hi = max(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  )

panelB <- ggplot(
  post_active_sc2,
  aes(
    x = coverage,
    y = mean_active_infected,
    color = factor(
      efficacy,
      levels = target_efficacies,
      labels = scales::percent(target_efficacies, accuracy = 1)
    ),
    group = efficacy
  )
) +
  geom_line() +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, NA)
  ) +
  geom_pointrange(
    aes(ymin = low, ymax = hi)
  ) +
  x_percent_scale +
  scale_color_manual(values = cols_b) +
  figure_theme +
  labs(
    x = "Coverage",
    y = "Active infection\nprevalence (%)",
    colour = "Efficacy"
  )

# Panel C (SC2 heatmap)
heat_data_c <- reductions_summary |>
  filter(
    coverage %in% target_vals,
    efficacy %in% target_vals,
    archetype == "sars_cov_2"
  )

panelC <- ggplot(
  heat_data_c,
  aes(
    x = factor(coverage, levels = target_vals, labels = scales::percent(target_vals)),
    y = factor(efficacy, levels = target_vals, labels = scales::percent(target_vals)),
    fill = mean_incidence_reduction
  )
) +
  geom_tile() +
  coord_fixed(ratio = 1) +
  ggplot2::scale_fill_viridis_c(
    option = "mako",
    direction = -1,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  figure_theme +
  theme(panel.grid = element_blank(), legend.position = "right") +
  labs(
    x = "Coverage",
    y = "Efficacy",
    fill = "Annual incidence\nreduction (%)"
  )

# Panel D & E (flu lines)
cols_d <- c("#E68996", "#D93052", "#9D374C")
plot_data_d <- reductions_summary |>
  filter(
    efficacy %in% target_efficacies,
    archetype == "flu",
    coverage %in% target_coverage_intervals
  )

panelD <- ggplot(
  plot_data_d,
  aes(
    x = coverage,
    y = mean_incidence_reduction,
    color = factor(
      efficacy,
      levels = target_efficacies,
      labels = scales::percent(target_efficacies, accuracy = 1)
    ),
    group = efficacy
  )
) +
  geom_line() +
  geom_pointrange(
    aes(ymin = low_incidence, ymax = hi_incidence)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  x_percent_scale +
  scale_color_manual(values = cols_d) +
  figure_theme +
  labs(
    x = "Coverage",
    y = "Annual incidence\nreduction (%)",
    colour = "Efficacy"
  )

cols_e <- cols_d
post_active_flu <- metrics |>
  filter(
    window == "post",
    archetype == "flu",
    efficacy %in% target_efficacies,
    coverage %in% target_coverage_intervals
  ) |>
  group_by(archetype, coverage, efficacy) |>
  summarise(
    mean_active_infected = mean(mean_prevalence, na.rm = TRUE),
    low = min(mean_prevalence, na.rm = TRUE),
    hi = max(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  )

panelE <- ggplot(
  post_active_flu,
  aes(
    x = coverage,
    y = mean_active_infected,
    color = factor(
      efficacy,
      levels = target_efficacies,
      labels = scales::percent(target_efficacies, accuracy = 1)
    ),
    group = efficacy
  )
) +
  geom_line() +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 1.5)
  ) +
  geom_pointrange(
    aes(ymin = low, ymax = hi)
  ) +
  x_percent_scale +
  scale_color_manual(values = cols_e) +
  figure_theme +
  labs(
    x = "Coverage",
    y = "Active infection\nprevalence (%)",
    colour = "Efficacy"
  )

# Panel F (flu heatmap)
heat_data_f <- reductions_summary |>
  filter(
    coverage %in% target_vals,
    efficacy %in% target_vals,
    archetype == "flu"
  )
panelF <- ggplot(
  heat_data_f,
  aes(
    x = factor(coverage, levels = target_vals, labels = scales::percent(target_vals)),
    y = factor(efficacy, levels = target_vals, labels = scales::percent(target_vals)),
    fill = mean_incidence_reduction
  )
) +
  geom_tile() +
  coord_fixed(ratio = 1) +
  ggplot2::scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  figure_theme +
  theme(panel.grid = element_blank(), legend.position = "right") +
  labs(
    x = "Coverage",
    y = "Efficacy",
    fill = "Annual incidence\nreduction (%)"
  )

# Grid assembly
complete_combined_plot <- (
  (panelA + (panelB + theme(legend.position = "none")) + panelC) /
    (panelD + (panelE + theme(legend.position = "none")) + panelF) +
    plot_annotation(tag_levels = "A")
) & theme(plot.margin = margin(2, 2, 2, 2, unit = "pt"))

ggsave(
  filename = here::here("figures", "endemic_scenario", "plot.png"),
  plot = complete_combined_plot,
  width = 9.5,
  height = 4.5,
  units = "in",
  dpi = 600
)

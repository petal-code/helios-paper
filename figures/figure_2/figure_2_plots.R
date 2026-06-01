library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(viridis)
library(cowplot)

dir_sim <- "/helios-paper/figures/figure_2/figure_2_simulations"

files <- list.files(
  path = dir_sim,
  pattern = "\\.rds$",
  full.names = TRUE
)

read_simulation <- function(filepath) {
  simulation_file <- readRDS(filepath)
  dt <- simulation_file$parameters$dt
  pop_size <- 50000

  simulation_file$simulation %>%
    mutate(
      days = timestep * dt,
      year = floor(days / 365),
      filename = basename(filepath),
      archetype = simulation_file$parameters$archetype_label,
      coverage = simulation_file$parameters$coverage,
      efficacy = simulation_file$parameters$efficacy,
      active_infected = E_count + I_count,
      prevalence = (active_infected / pop_size) * 100
    ) %>%
    group_by(year, filename, archetype, coverage, efficacy) %>%
    summarise(
      total_infections = sum(E_new, na.rm = TRUE),
      annualized_incidence_rate = total_infections / pop_size,
      mean_active_infected = mean(active_infected, na.rm = TRUE),
      mean_prevalence = mean(prevalence, na.rm = TRUE),
      .groups = "drop"
    )
}

all_sims <- purrr::map_dfr(files, read_simulation)

metrics <- all_sims %>%
  mutate(
    window = case_when(
      year %in% 5:7 ~ "baseline",
      year %in% 15:17 ~ "post"
    )
  ) %>%
  filter(!is.na(window)) %>%
  group_by(filename, archetype, coverage, efficacy, window) %>%
  summarise(
    mean_incidence_rate = mean(annualized_incidence_rate, na.rm = TRUE),
    mean_active_infected = mean(mean_active_infected, na.rm = TRUE),
    mean_prevalence = mean(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  )

reductions <- metrics %>%
  pivot_wider(
    names_from = window,
    values_from = c(mean_incidence_rate, mean_active_infected, mean_prevalence)
  ) %>%
  mutate(
    incidence_reduction = 1 -
      mean_incidence_rate_post / mean_incidence_rate_baseline,
    active_infected_reduction = 1 -
      mean_active_infected_post / mean_active_infected_baseline,
    prevalence_reduction = 1 - mean_prevalence_post / mean_prevalence_baseline
  )

reductions_summary <- reductions %>%
  group_by(archetype, coverage, efficacy) %>%
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

# PANEL A & B (SC2 Lines)
cols_a <- c("#5083DB", "#395D9C", "#253B65")
plot_data_a <- reductions_summary %>%
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
      labels = scales::percent(target_efficacies, accuracy = 1)
    ),
    group = efficacy
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = low_incidence, ymax = hi_incidence),
    width = 0.02,
    alpha = 0.5
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  x_percent_scale +
  scale_color_manual(values = cols_a) +
  theme_minimal() +
  labs(
    x = "AQI Coverage",
    y = "% Reduction in Annualized \n  Disease Incidence",
    colour = "Efficacy"
  )

cols_b <- c("#5083DB", "#395D9C", "#253B65")
post_active_sc2 <- metrics %>%
  filter(
    window == "post",
    archetype == "sars_cov_2",
    efficacy %in% target_efficacies,
    coverage %in% target_coverage_intervals
  ) %>%
  group_by(archetype, coverage, efficacy) %>%
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
      labels = scales::percent(target_efficacies, accuracy = 1)
    ),
    group = efficacy
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, NA)
  ) +
  geom_errorbar(aes(ymin = low, ymax = hi), width = 0.02, alpha = 0.5) +
  x_percent_scale +
  scale_color_manual(values = cols_b) +
  theme_minimal() +
  labs(
    x = "AQI Coverage",
    y = "Active Infection Prevalence",
    colour = "Efficacy"
  )

# PANEL C (SC2 Heatmap)
heat_data_c <- reductions_summary %>%
  filter(
    coverage %in% target_vals,
    efficacy %in% target_vals,
    archetype == "sars_cov_2"
  )

panelC <- ggplot(
  heat_data_c,
  aes(
    x = factor(coverage, labels = scales::percent(target_vals)),
    y = factor(efficacy, labels = scales::percent(target_vals)),
    fill = mean_incidence_reduction
  )
) +
  geom_tile(color = "white") +
  viridis::scale_fill_viridis(
    option = "mako",
    direction = -1,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(
    x = "AQI Coverage",
    y = "AQI Efficacy",
    fill = "% Reduction in \n Annualized Disease\n Incidence"
  )

# PANEL D & E (Flu Lines)
cols_d <- c("#E68996", "#D93052", "#9D374C")
plot_data_d <- reductions_summary %>%
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
      labels = scales::percent(target_efficacies, accuracy = 1)
    ),
    group = efficacy
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = low_incidence, ymax = hi_incidence),
    width = 0.02,
    alpha = 0.5
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  x_percent_scale +
  scale_color_manual(values = cols_d) +
  theme_minimal() +
  labs(
    x = "AQI Coverage",
    y = "% Reduction in Annualized \n  Disease Incidence",
    colour = "Efficacy"
  )

cols_e <- c("#E68996", "#D93052", "#9D374C")
post_active_flu <- metrics %>%
  filter(
    window == "post",
    archetype == "flu",
    efficacy %in% target_efficacies,
    coverage %in% target_coverage_intervals
  ) %>%
  group_by(archetype, coverage, efficacy) %>%
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
      labels = scales::percent(target_efficacies, accuracy = 1)
    ),
    group = efficacy
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 1.5)
  ) +
  geom_errorbar(aes(ymin = low, ymax = hi), width = 0.02, alpha = 0.5) +
  x_percent_scale +
  scale_color_manual(values = cols_e) +
  theme_minimal() +
  labs(
    x = "AQI Coverage",
    y = "Active Infection Prevalence",
    colour = "Efficacy"
  )

#  PANEL F (Flu Heatmap)
heat_data_f <- reductions_summary %>%
  filter(
    coverage %in% target_vals,
    efficacy %in% target_vals,
    archetype == "flu"
  )
panelF <- ggplot(
  heat_data_f,
  aes(
    x = factor(coverage, labels = scales::percent(target_vals)),
    y = factor(efficacy, labels = scales::percent(target_vals)),
    fill = mean_incidence_reduction
  )
) +
  geom_tile(color = "white") +
  viridis::scale_fill_viridis(
    option = "magma",
    direction = -1,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(
    x = "AQI Coverage",
    y = "AQI Efficacy",
    fill = "% Reduction in \nAnnualized Disease \nIncidence"
  )

# Grid Assembly

# 1. Extract Line (Discrete) Legends
leg_sc2_discrete <- get_legend(
  panelA + theme(legend.box.margin = margin(0, 0, 0, 10))
)
leg_flu_discrete <- get_legend(
  panelD + theme(legend.box.margin = margin(0, 0, 0, 10))
)

# 2. Extract Heatmap (Continuous) Legends
leg_sc2_heat <- get_legend(panelC + theme(legend.position = "right"))
leg_flu_heat <- get_legend(panelF + theme(legend.position = "right"))

# 3. Create a tight sub-grid for SC2 legends
sc2_legend_group <- plot_grid(
  leg_sc2_discrete,
  leg_sc2_heat,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)

# 4. Create a tight sub-grid for Flu legends
flu_legend_group <- plot_grid(
  leg_flu_discrete,
  leg_flu_heat,
  ncol = 1,
  rel_heights = c(1, 1),
  align = "v"
)

# 5. Stack the two groups with space (rel_heights) between them
side_legends <- plot_grid(
  sc2_legend_group,
  flu_legend_group,
  ncol = 1,
  rel_heights = c(1, 1) # This ensures SC2 stays top and Flu stays bottom
)

# 6. Create main rows (same as before)
row1 <- plot_grid(
  panelA + theme(legend.position = "none"),
  panelB + theme(legend.position = "none"),
  panelC + theme(legend.position = "none"),
  nrow = 1,
  labels = c("A", "B", "C"),
  rel_widths = c(1, 1, 1)
)

row2 <- plot_grid(
  panelD + theme(legend.position = "none"),
  panelE + theme(legend.position = "none"),
  panelF + theme(legend.position = "none"),
  nrow = 1,
  labels = c("D", "E", "F"),
  rel_widths = c(1, 1, 1)
)

# 7. Final Combined Layout
complete_combined_plot <- plot_grid(
  plot_grid(row1, row2, nrow = 2),
  side_legends,
  ncol = 2,
  rel_widths = c(1, 0.2)
)

complete_combined_plot

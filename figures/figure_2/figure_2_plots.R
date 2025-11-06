library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(viridis)
library(cowplot)


dir_sim <- "/Users/geethaj/Downloads/figure_2_simulations"

files <- list.files(
  path = dir_sim, 
  pattern = "\\.rds$",
  full.names = TRUE
)


read_simulation <- function(filepath) {
  simulation_file <- readRDS(filepath)
  dt <- simulation_file$parameters$dt
  pop_size <- 100000
  
  simulation_file$simulation %>%
    mutate(
      days = timestep * dt,
      year = floor(days / 365),
      filename = basename(filepath),
      archetype = simulation_file$parameters$archetype_label,
      coverage  = simulation_file$parameters$coverage,
      efficacy  = simulation_file$parameters$efficacy,
      active_infected = E_count + I_count,
      prevalence = (active_infected / pop_size)*100
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

# Baseline (years 6-10) vs Post (years 11-15)

metrics <- all_sims %>%
  mutate(window = case_when(
    year %in% 6:10   ~ "baseline",
    year %in% 11:15 ~ "post"
  )) %>%
  filter(!is.na(window)) %>%
  group_by(filename, archetype, coverage, efficacy, window) %>%
  summarise(
    mean_incidence_rate   = mean(annualized_incidence_rate, na.rm = TRUE),
    mean_active_infected  = mean(mean_active_infected, na.rm = TRUE),
    mean_prevalence       = mean(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  )


reductions <- metrics %>%
  pivot_wider(
    names_from = window,
    values_from = c(mean_incidence_rate, mean_active_infected, mean_prevalence)
  ) %>%
  mutate(
    incidence_reduction       = 1 - mean_incidence_rate_post / mean_incidence_rate_baseline,
    active_infected_reduction = 1 - mean_active_infected_post / mean_active_infected_baseline,
    prevalence_reduction      = 1 - mean_prevalence_post / mean_prevalence_baseline
  )

#Summarize Reductions
reductions_summary <- reductions %>%
  group_by(archetype, coverage, efficacy) %>%
  summarise (
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

target_coverage_intervals <- c(0.2, 0.4, 0.6, 0.8, 1.0)
x_percent_scale <- scale_x_continuous(
  labels = scales::percent_format(accuracy = 1),
  limits = c(0.2, 1.0),
  breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)
)
# Panel A: % Reduction in incidence
cols <- c("#5083DB", "#395D9C", "#253B65")

target_efficacies <- c(0.4, 0.6, 0.8)

plot_data <- reductions_summary %>%
  filter(archetype == "sars_cov_2") %>%
  mutate(
    efficacy = round(efficacy, 2),
    coverage = round(coverage, 2)
  ) %>%
  filter(efficacy %in% target_efficacies,
         coverage %in% target_coverage_intervals) %>%
  mutate(efficacy_factor = factor(
    efficacy,
    levels = target_efficacies,
    labels = scales::percent(target_efficacies, accuracy = 1)
  ))

panelA <- ggplot(plot_data,
                 aes(x = coverage, y = mean_incidence_reduction,
                     color = efficacy_factor, group = efficacy_factor)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = low_incidence, ymax = hi_incidence),
                width = 0.02, alpha = 0.5) +
  labs(
    x = "UV-C Coverage",
    y = "% Reduction in Annualized \nDisease Incidence",
    colour = "Efficacy"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  x_percent_scale +
  scale_color_manual(values = cols) +
  theme_minimal()
panelA

# Panel B: Active Infection Prevalence (post period only)
cols <- c("#83d8b4", "#3BB585", "#1d5d43")
target_efficacies <- c(0.4, 0.6, 0.8)
target_b_coverage_intervals <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)

# Summarise post-intervention prevalence
post_active <- metrics %>%
  filter(window == "post", archetype == "sars_cov_2") %>%
  group_by(archetype, coverage, efficacy) %>%
  summarise(
    mean_active_infected = mean(mean_prevalence, na.rm = TRUE),
    low = min(mean_prevalence, na.rm = TRUE),
    hi = max(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage = round(coverage, 2),
    efficacy = round(efficacy, 2)
  )

# Filter and label efficacy levels
plot_data_b <- post_active %>%
  filter(efficacy %in% target_efficacies,
         coverage %in% target_b_coverage_intervals) %>%
  mutate(efficacy_factor = factor(
    efficacy,
    levels = target_efficacies,
    labels = scales::percent(target_efficacies, accuracy = 1)
  ))

# Plot
panelB <- ggplot(plot_data_b,
                 aes(x = coverage,
                     y = mean_active_infected,
                     color = efficacy_factor,
                     group = efficacy_factor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = low, ymax = hi),
                width = 0.02, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, NA)) +
  x_percent_scale +
  scale_color_manual(values = cols) +
  labs(
    x = "UV-C Coverage",
    y = "Active Infection Prevalence",
    colour = "Efficacy"
  ) +
  theme_minimal()

panelB

#Panel C: Heatmaps
target_vals <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)

heat_data <- reductions_summary %>%
  filter(archetype == "sars_cov_2") %>%
  mutate(
    coverage = round(coverage, 2),
    efficacy = round(efficacy, 2)
  ) %>%
  filter(coverage %in% target_vals,
         efficacy %in% target_vals)

# Factors for plotting (bottom to top = increasing efficacy)
heat_data <- heat_data %>%
  mutate(
    coverage_factor = factor(
      coverage,
      levels = target_vals,
      labels = scales::percent(target_vals, accuracy = 1)
    ),
    efficacy_factor = factor(
      efficacy,
      levels = target_vals,  
      labels = scales::percent(target_vals, accuracy = 1)
    )
  )

panelC <- ggplot(heat_data,
                 aes(x = coverage_factor,
                     y = efficacy_factor,
                     fill = mean_incidence_reduction)) +
  geom_tile(color = "white") +
  viridis::scale_fill_viridis(
    option = "mako",
    direction = -1,
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "UV-C Coverage",
    y = "UV-C Efficacy",
    fill = "% Reduction in \n Annualized Disease \n Incidence"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black")
  )

panelC


#Combined SC2 Plots
combined_sc2_plot <- plot_grid(
  panelA, panelB, panelC,
  nrow = 1,
  labels = c("A", "B", "C"),
  rel_widths = c(1, 1, 1.25)  
)

combined_sc2_plot


#Panel D: Flu 
cols <- c("#CD86EA", "#651983", "#3F1052")

plot_data_d <- reductions_summary %>%
  filter(archetype == "flu") %>%
  mutate(
    efficacy = round(efficacy, 2),
    coverage = round(coverage, 2)
  ) %>%
  filter(efficacy %in% target_efficacies,
         coverage %in% target_coverage_intervals) %>%
  mutate(efficacy_factor = factor(
    efficacy,
    levels = target_efficacies,
    labels = scales::percent(target_efficacies, accuracy = 1)
  ))

panelD <- ggplot(plot_data_d,
                 aes(x = coverage, y = mean_incidence_reduction,
                     color = efficacy_factor, group = efficacy_factor)) +
  geom_line(size = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = low_incidence, ymax = hi_incidence),
                width = 0.02, alpha = 0.5) +
  labs(
    x = "UV-C Coverage",
    y = "% Reduction in Annualized \nDisease Incidence",
    colour = "Efficacy"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  x_percent_scale +
  scale_color_manual(values = cols) +
  theme_minimal()
panelD


# Panel E: Flu Active Infections (post period only)
cols <- c("#E68996", "#D93052", "#9D374C")

target_efficacies <- c(0.4, 0.6, 0.8)
target_e_coverage_intervals <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)

# Summarise post-intervention prevalence
post_active <- metrics %>%
  filter(window == "post", archetype == "flu") %>%
  group_by(archetype, coverage, efficacy) %>%
  summarise(
    mean_active_infected = mean(mean_prevalence, na.rm = TRUE),
    low = min(mean_prevalence, na.rm = TRUE),
    hi = max(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage = round(coverage, 2),
    efficacy = round(efficacy, 2)
  )

# Filter and label efficacy levels
plot_data_e <- post_active %>%
  filter(efficacy %in% target_efficacies,
         coverage %in% target_b_coverage_intervals) %>%
  mutate(efficacy_factor = factor(
    efficacy,
    levels = target_efficacies,
    labels = scales::percent(target_efficacies, accuracy = 1)
  ))

# Plot
panelE <- ggplot(plot_data_e,
                 aes(x = coverage,
                     y = mean_active_infected,
                     color = efficacy_factor,
                     group = efficacy_factor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = low, ymax = hi),
                width = 0.02, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, NA)) +
  x_percent_scale +
  scale_color_manual(values = cols) +
  labs(
    x = "UV-C Coverage",
    y = "Active Infection Prevalence",
    colour = "Efficacy"
  ) +
  theme_minimal()

panelE

#Panel F: Flu Heatmap
target_vals <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)

heat_data <- reductions_summary %>%
  filter(archetype == "flu") %>%
  mutate(
    coverage = round(coverage, 2),
    efficacy = round(efficacy, 2)
  ) %>%
  filter(coverage %in% target_vals,
         efficacy %in% target_vals)

heat_data <- heat_data %>%
  mutate(
    coverage_factor = factor(
      coverage,
      levels = target_vals,
      labels = scales::percent(target_vals, accuracy = 1)
    ),
    efficacy_factor = factor(
      efficacy,
      levels = target_vals,  
      labels = scales::percent(target_vals, accuracy = 1)
    )
  )

panelF <- ggplot(heat_data,
                 aes(x = coverage_factor,
                     y = efficacy_factor,
                     fill = mean_incidence_reduction)) +
  geom_tile(color = "white") +
  viridis::scale_fill_viridis(
    option = "magma",
    direction = -1,
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "UV-C Coverage",
    y = "UV-C Efficacy",
    fill = "% Reduction in \n Annualized Disease \n Incidence"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black")
  )

panelF

#Combined flu Plots
combined_flu_plot <- plot_grid(
  panelD, panelE, panelF,
  nrow = 1,
  labels = c("D", "E", "F"),
  rel_widths = c(1, 1, 1.25)  
)


complete_combined_plot <- plot_grid(combined_sc2_plot, combined_flu_plot, nrow = 2)
complete_combined_plot




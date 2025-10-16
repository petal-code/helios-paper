library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(viridis)
library(cowplot)


dir_sim <- "/Users/geethaj/documents/helios_files/figure_2_simulations_20092025"

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

# Baseline (years 6–8) vs Post (years 18–20)

metrics <- all_sims %>%
  mutate(window = case_when(
    year %in% 5:7   ~ "baseline",
    year %in% 18:20 ~ "post"
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

#Adjust to report percent instead of proportion
x_percent_scale <- scale_x_continuous(
  labels = scales::percent_format(accuracy = 1),
  limits = c(0.2, 1.0),
  breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)
)

target_coverage_intervals <- c(0.2, 0.4, 0.6, 0.8, 1.0)
# Panel A: % Reduction in incidence
cols <- c("#5083DB", "#395D9C", "#253B65")

target_efficacies <- c(0.4, 0.6, 0.8)

plot_data <- reductions_summary %>% 
  filter(efficacy %in% target_efficacies, archetype == "sars_cov_2") %>% 
  filter(coverage %in% target_coverage_intervals)

panelA <- ggplot(plot_data, 
                 aes(x = coverage, y = mean_incidence_reduction,
                     color = factor(efficacy, labels = scales::percent(target_efficacies, accuracy = 1)), group = efficacy)) + 
  geom_line(size  = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = low_incidence, ymax = hi_incidence),
                width = 0.02, alpha = 0.5) +
  labs(
    x = "UV-C Coverage",
    y = "% Reduction in Annualized \n  Disease Incidence",
    colour = "Efficacy"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  x_percent_scale +
  scale_color_manual(values = cols)  +
  theme_minimal()


# Panel B: Active Infection Prevalence (post period only)
cols <- c("#83d8b4", "#3BB585", "#1d5d43")

post_active <- metrics %>%
  filter(window == "post", archetype == "sars_cov_2") %>%
  group_by(archetype, coverage, efficacy) %>%
  summarise(
    mean_active_infected = mean(mean_prevalence, na.rm = TRUE),
    low = min(mean_prevalence, na.rm = TRUE),
    hi = max(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  )

plot_data_b <- post_active %>% 
  filter(efficacy %in% target_efficacies) %>% 
  filter(coverage %in% target_coverage_intervals)

panelB <- ggplot(plot_data_b,
                 aes(x = coverage, y = mean_active_infected,
                     color = factor(efficacy, labels = scales::percent(target_efficacies, accuracy = 1)), group = efficacy)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous (labels = scales::percent_format(scale = 1)) + 
  geom_errorbar(aes(ymin = low, ymax = hi), width = 0.02, alpha = 0.5) +
  labs(
    x = "UV-C Coverage",
    y = "Active Infection Prevalence",
    colour = "Efficacy"
  ) +
  x_percent_scale +
  scale_color_manual(values = cols)  +
  theme_minimal()
#Panel C: Heatmaps
target_vals <- c(0.2, 0.4, 0.6, 0.8, 1.0)

heat_data <- reductions_summary %>%
  filter(coverage %in% target_vals,
         efficacy %in% target_vals, archetype == "sars_cov_2")

panelC <- ggplot(heat_data, aes(
  x = factor(coverage, labels = scales::percent(c(0.2, 0.4, 0.6, 0.8, 1.0))),
  y = factor(efficacy, labels = scales::percent(c(0.2, 0.4, 0.6, 0.8, 1.0))),
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
  theme(panel.grid = element_blank())


#Combined SC2 Plots
combined_sc2_plot <- plot_grid(panelA, panelB, panelC, nrow = 1, labels = c("A", "B", "C"))
combined_sc2_plot


#Panel D: Flu 
cols <- c("#CD86EA", "#651983", "#3F1052")

target_efficacies <- c(0.4, 0.6, 0.8)

plot_data <- reductions_summary %>% 
  filter(efficacy %in% target_efficacies, archetype == "flu") %>% 
  filter(coverage %in% target_coverage_intervals)

panelD <- ggplot(plot_data, 
                 aes(x = coverage, y = mean_incidence_reduction,
                     color = factor(efficacy, labels = scales::percent(target_efficacies, accuracy = 1)), group = efficacy)) + 
  geom_line(size  = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = low_incidence, ymax = hi_incidence),
                width = 0.02, alpha = 0.5) +
  labs(
    x = "UV-C Coverage",
    y = "% Reduction in  Annualized \n   Disease Incidence",
    colour = "Efficacy"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  x_percent_scale +
  scale_color_manual(values = cols)  +
  theme_minimal()

# Panel E: Flu Active Infections (post period only)
cols <- c("#E68996", "#D93052", "#9D374C")

post_active <- metrics %>%
  filter(window == "post", archetype == "flu") %>%
  group_by(archetype, coverage, efficacy) %>%
  summarise(
    mean_active_infected = mean(mean_prevalence, na.rm = TRUE),
    low = min(mean_prevalence, na.rm = TRUE),
    hi = max(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  )

plot_data <- post_active %>% 
  filter(efficacy %in% target_efficacies) %>% 
  filter(coverage %in% target_coverage_intervals)

panelE <- ggplot(plot_data,
                 aes(x = coverage, y = mean_active_infected,
                     color = factor(efficacy, labels = scales::percent(target_efficacies, accuracy = 1)), group = efficacy)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous (labels = scales::percent_format(scale = 1)) + 
  geom_errorbar(aes(ymin = low, ymax = hi), width = 0.02, alpha = 0.5) +
  labs(
    x = "UV-C Coverage",
    y = "Active Infection Prevalence",
    colour = "Efficacy"
  ) +
  x_percent_scale +
  scale_color_manual(values = cols)  +
  theme_minimal()

#Panel F: Flu Heatmap
target_vals <- c(0.2, 0.4, 0.6, 0.8, 1.0)

heat_data <- reductions_summary %>%
  filter(coverage %in% target_vals,
         efficacy %in% target_vals, archetype == "flu")

panelF <- ggplot(heat_data, aes(x = factor(coverage, labels = scales::percent(c(0.2, 0.4, 0.6, 0.8, 1.0))),
                                y = factor(efficacy, labels = scales::percent(c(0.2, 0.4, 0.6, 0.8, 1.0))),,
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
  theme(panel.grid = element_blank())

#Combined flu Plots
combined_flu_plot <- plot_grid(panelD, panelE, panelF, nrow = 1, labels = c("D", "E", "F"))
combined_flu_plot

complete_combined_plot <- plot_grid(combined_sc2_plot, combined_flu_plot, nrow = 2)
complete_combined_plot

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(viridis)
library(cowplot)


dir_sim <- "/Users/geethaj/documents/helios_files/figure_3_simulations"

files <- list.files(
  path = dir_sim,
  pattern = "\\.rds$",
  full.names = TRUE
)

read_simulation <- function(filepath) {
  sim <- readRDS(filepath)
  
  dt <- sim$parameters$dt
  N  <- sim$parameters$human_population
  
  sim$simulation %>%
    mutate(
      days      = timestep * dt,
      year      = floor(days / 365) + 1,
      filename  = basename(filepath),
      archetype = sim$parameters$archetype_label,
      coverage  = sim$parameters$coverage,
      efficacy  = sim$parameters$efficacy,
      coverage_type = sim$parameters$far_uvc_joint_coverage_type,
      active_infected = E_count + I_count,
      prevalence = active_infected / N
    ) %>%
    group_by(year, filename, archetype, coverage, efficacy, coverage_type) %>%
    summarise(
      annualized_incidence = sum(E_new, na.rm = TRUE) / N,
      mean_active_prevalence = mean(prevalence, na.rm = TRUE),
      .groups = "drop"
    )
}

all_sims <- map_dfr(files, read_simulation)

# Define baseline (years 5–7) vs post (years 18–20) 
metrics <- all_sims %>% 
  mutate(window = case_when(
    year %in% 5:7   ~ "baseline",
    year %in% 18:20 ~ "post",
    TRUE ~ NA_character_
  )) %>% 
  filter(!is.na(window)) %>% 
  group_by(filename, archetype, coverage, efficacy, coverage_type, window) %>%
  summarise(
    mean_annualized_incidence = mean(annualized_incidence, na.rm = TRUE),
    mean_prevalence = mean(mean_active_prevalence, na.rm = TRUE),
    .groups = "drop"
  )

reductions <- metrics %>%
  pivot_wider(
    names_from = window,
    values_from = c(mean_annualized_incidence, mean_prevalence)
  ) %>%
  mutate(
    pct_reduction_incidence =
      100 * (mean_annualized_incidence_baseline - mean_annualized_incidence_post) /
      mean_annualized_incidence_baseline,
    
    pct_reduction_prevalence =
      100 * (mean_prevalence_baseline - mean_prevalence_post) /
      mean_prevalence_baseline
  )

#Panel A: % reduction in annualized incidence - SC2
panel_a <- reductions %>%
  filter(archetype == "sars_cov_2", efficacy == 0.6) %>%  # optional pathogen filter
  ggplot(aes(x = factor(coverage),
             y = pct_reduction_incidence,
             fill = coverage_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("random" = "#6BAED6",        # light blue
               "targeted_riskiness" = "#08519C"),  # dark blue
    labels = c("random" = "Random", "targeted_riskiness" = "Targeted")
  ) +
  labs(
    x = "Coverage",
    y = "% Reduction in Annualized Disease Incidence\n(relative to baseline)",
    fill = "Coverage Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

panel_a

# Panel B: Relative Extra Impact of Targeted vs Random - SC2
#plot b/a, 
panel_b_data <- reductions %>%
  select(archetype, coverage, efficacy, coverage_type, pct_reduction_incidence) %>%
  pivot_wider(
    names_from = coverage_type,
    values_from = pct_reduction_incidence,
    values_fn = mean   
  ) %>%
  mutate(
    rel_extra_impact = targeted_riskiness / random  
  )

panel_b <- panel_b_data %>%
  filter(efficacy == 0.6, archetype =="sars_cov_2") %>%
  ggplot(aes(x = factor(coverage), y = rel_extra_impact)) +
  geom_col(fill = "#08519C", width = 0.6) +
  labs(
    x = "Coverage",
    y = "Targeted / Random Reduction Ratio"
    #title = "Panel B: Relative Impact of Targeted vs Random"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

panel_b

#Panel C: % reduction in annualized incidence - flu
panel_c <- reductions %>%
  filter(archetype == "flu", efficacy == 0.6) %>%  # optional pathogen filter
  ggplot(aes(x = factor(coverage),
             y = pct_reduction_incidence,
             fill = coverage_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("random" = "#ffc532",        
               "targeted_riskiness" = "#a12424"),  
    labels = c("random" = "Random", "targeted_riskiness" = "Targeted")
  ) +
  labs(
    x = "Coverage",
    y = "% Reduction in Annualized Disease Incidence\n(relative to baseline)",
    fill = "Coverage Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

panel_c

#Panel D: Ratio of reduction - flu
#plot b/a, 
panel_d_data <- reductions %>%
  select(archetype, coverage, efficacy, coverage_type, pct_reduction_incidence) %>%
  pivot_wider(
    names_from = coverage_type,
    values_from = pct_reduction_incidence,
    values_fn = mean   
  ) %>%
  mutate(
    rel_extra_impact = targeted_riskiness / random  
  )

panel_d <- panel_d_data %>%
  filter(efficacy == 0.6, archetype =="flu") %>%
  ggplot(aes(x = factor(coverage), y = rel_extra_impact)) +
  geom_col(fill = "#a12424", width = 0.6) +
  labs(
    x = "Coverage",
    y = "Targeted / Random Reduction Ratio"
    #title = "Panel d: Relative Impact of Targeted vs Random"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

panel_d

combined_sc2_plot <- plot_grid(panel_a, panel_b, nrow = 1, labels = c("A", "B"))
combined_sc2_plot

combined_flu_plot <- plot_grid(panel_c, panel_d, nrow = 1, labels = c("C", "D"))
combined_flu_plot

complete_combined_plot <- plot_grid(combined_sc2_plot, combined_flu_plot, nrow = 2)
complete_combined_plot




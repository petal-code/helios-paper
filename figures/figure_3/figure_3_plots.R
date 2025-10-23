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


#Panel A: Random vs Targeting

# Figure 2.2 — Random vs riskiness-targeted location selection

library(helios)
library(ggplot2)
library(dplyr)

theme_set(theme_bw())

# palette used in the report
#blueprint_colours <- colorRampPalette(c("#00AFFF", "#03113E"))(4)

# Parameters to generate location riskiness across setting types
scaling_factor <- 5
parameter_list <- get_parameters(
  archetype = "flu",
  overrides = list(
    seed = 42,
    human_population = 400000 / scaling_factor,
    number_initial_S = 240000 / scaling_factor,
    number_initial_E = 160000 / scaling_factor,
    number_initial_I = 0,
    number_initial_R = 0,
    simulation_time = 150
  )
) |>
  set_setting_specific_riskiness(
    setting = "school", mean = 0, sd = 0.3544,
    min = 1 / sqrt(4.75), max = sqrt(4.75)
  ) |>
  set_setting_specific_riskiness(
    setting = "workplace", mean = 0, sd = 0.5072,
    min = 1 / sqrt(6.35), max = sqrt(6.35)
  ) |>
  set_setting_specific_riskiness(
    setting = "household", mean = 0, sd = 0.0871,
    min = 1 / sqrt(2.5), max = sqrt(2.5)
  ) |>
  set_setting_specific_riskiness(
    setting = "leisure", mean = 0, sd = 0.4278,
    min = 1 / sqrt(5.5), max = sqrt(5.5)
  )

vars <- create_variables(parameter_list)
parameters <- vars$parameters_list
variables  <- vars$variables_list

# Helper to extract vectors for plotting from a parameters list
extract_df <- function(p_list, v_list, label) {
  sw <- generate_far_uvc_switches(parameters_list = p_list, variables_list = v_list)
  data.frame(
    setting_type = c(rep("school",   length(sw$school_specific_riskiness)),
                     rep("leisure",  length(sw$leisure_specific_riskiness)),
                     rep("workplace",length(sw$workplace_specific_riskiness))),
    riskiness = c(sw$school_specific_riskiness,
                  sw$leisure_specific_riskiness,
                  sw$workplace_specific_riskiness),
    uvc = c(sw$uvc_school, sw$uvc_leisure, sw$uvc_workplace),
    type = label
  )
}

# Random targeting at 50% square footage
p_rand <- set_uvc(
  parameters_list = parameters, setting = "joint",
  coverage = 0.5, coverage_target = "square_footage",
  coverage_type = "random", efficacy = 0.8, timestep = 1
)

# Riskiness-targeted at 50% square footage
p_targ <- set_uvc(
  parameters_list = parameters, setting = "joint",
  coverage = 0.5, coverage_target = "square_footage",
  coverage_type = "targeted_riskiness", efficacy = 0.8, timestep = 1
)

df <- bind_rows(
  extract_df(p_rand, variables, "Random Targeting of Far UVC"),
  extract_df(p_targ, variables, "Targeting Far UVC Based on Riskiness")
) |>
  arrange(desc(riskiness)) |>
  mutate(rank = row_number(),
         installed = ifelse(uvc == 1, "Yes", "No"))

ggplot(df, aes(x = rank, y = riskiness, fill = installed, colour = installed)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ type) +
  scale_colour_manual(values = c("No" = "grey50", "Yes" = "#03113E")) +
  scale_fill_manual(values   = c("No" = "grey80", "Yes" = "#03113E")) +
  labs(x = "Rank Order of Riskiness",
       y = "Relative riskiness",
       fill = "Far UVC Installed?",
       colour = "Far UVC Installed?") +
  theme(legend.position = "bottom")


#Panel B: % reduction in annualized incidence - SC2
panel_b <- reductions %>%
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

panel_b

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




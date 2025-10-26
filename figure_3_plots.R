library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(viridis)
library(cowplot)



dir_sim <- "/Users/geethaj/documents/helios_files/figure_3_simulations"
param_combos <- readRDS("figures/figure_3/figure_3_parameter_combinations.rds")

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
      prevalence = active_infected / pop_size
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
all_sims <- all_sims %>%
  mutate(ID = as.numeric(str_extract(filename, "(?<=simulation_id_)\\d+")))

all_sims_labeled <- all_sims %>%
  left_join(param_combos, by = "ID")

all_sims_labeled <- all_sims_labeled %>%
  select(-ends_with(".y")) %>%
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x"))


# Baseline (years 3–5) vs Post (years 18–20)

metrics <- all_sims_labeled %>%
  mutate(window = case_when(
    year %in% 3:5   ~ "baseline",
    year %in% 18:20 ~ "post"
  )) %>%
  filter(!is.na(window)) %>%
  group_by(filename, archetype, coverage, efficacy, window, coverage_type) %>%
  summarise(
    mean_incidence_rate   = mean(annualized_incidence_rate, na.rm = TRUE),
    mean_active_infected  = mean(mean_active_infected, na.rm = TRUE),
    mean_prevalence       = mean(mean_prevalence, na.rm = TRUE),
    .groups = "drop"
  )

reductions <- metrics %>%
  pivot_wider(
    names_from = window,
    values_from = c(mean_incidence_rate,mean_active_infected, mean_prevalence)
  ) %>%
  mutate(
    incidence_reduction       = 1 - mean_incidence_rate_post / mean_incidence_rate_baseline,
    active_infected_reduction = 1 - mean_active_infected_post / mean_active_infected_baseline,
    prevalence_reduction      = 1 - mean_prevalence_post / mean_prevalence_baseline
  )

#Summarize Reductions
reductions_summary <- reductions %>%
  group_by(archetype, coverage, efficacy, coverage_type) %>%
  summarise(
    mean_incidence_reduction = mean(incidence_reduction, na.rm = TRUE),
    lo_incidence = quantile(incidence_reduction, 0.05, na.rm = TRUE),
    hi_incidence = quantile(incidence_reduction, 0.95, na.rm = TRUE),
    
    mean_active_reduction = mean(active_infected_reduction, na.rm = TRUE),
    lo_active = quantile(active_infected_reduction, 0.05, na.rm = TRUE),
    hi_active = quantile(active_infected_reduction, 0.95, na.rm = TRUE),
    
    mean_prevalence_reduction = mean(prevalence_reduction, na.rm = TRUE),
    lo_prev = quantile(prevalence_reduction, 0.05, na.rm = TRUE),
    hi_prev = quantile(prevalence_reduction, 0.95, na.rm = TRUE),
    .groups = "drop"
  )


# Panel A: Absolute reduction in annualized disease incidence targeted vs random implementation (compared to baseline)
panel_A_data <- reductions_summary %>%
  filter(efficacy == 0.5) %>%
  mutate(
    pathogen_label = case_when(
      archetype == "sars_cov_2" ~ "SARS-CoV-2",
      archetype == "flu" ~ "Influenza"
    ),
    combo_label = paste0(pathogen_label, " - ", ifelse(coverage_type == "random", "Random", "Targeted"))
  )

cols <- c(
  "SARS-CoV-2 - Random"   = "#9BCBF3",  
  "SARS-CoV-2 - Targeted" = "#2166AC",  
  "Influenza - Random"    = "#FDD99B",  
  "Influenza - Targeted"  = "#D6604D"   
)

ggplot(panel_A_data,
       aes(x = coverage,
           y = mean_incidence_reduction,
           color = combo_label,
           group = combo_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lo_incidence, ymax = hi_incidence),
                width = 0.015, alpha = 0.5) +
  scale_color_manual(values = cols, name = "") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0.2, 1.0),
                     breaks = seq(0.2, 1.0, 0.1)) +
  labs(
    title = "% Reduction in Annualized Disease Incidence (Efficacy = 0.5)",
    x = "Far-UVC Coverage (%)",
    y = "% Reduction (vs baseline)",
    color = "Pathogen × Coverage Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 15)
  )

# Panel B: Relative extra impact in annualized incidence


# Panel C: Heatmap of absolute impact (incidence, targeted - random)


target_vals <- c(0.2, 0.4, 0.6, 0.8, 1.0)

make_heatmap <- function(data, pathogen_label, title_label) {
  
  heat_data <- data %>%
    filter(archetype == pathogen_label,
           coverage %in% target_vals,
           efficacy %in% target_vals) %>%
    select(archetype, coverage, efficacy, coverage_type, mean_incidence_reduction) %>%
    pivot_wider(
      names_from = coverage_type,
      values_from = mean_incidence_reduction
    ) %>%
    mutate(diff_targeted_minus_random = targeted_riskiness - random)
  
  ggplot(heat_data, aes(
    x = factor(coverage, levels = target_vals),
    y = factor(efficacy, levels = target_vals),
    fill = diff_targeted_minus_random
  )) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%+.1f%%", diff_targeted_minus_random * 100)),
              size = 3.2, color = "white", fontface = "bold") +
    scale_fill_viridis(
      option = "mako",
      direction = -1,
      limits = c(0, 0.2),
      breaks = seq(0, 0.2, 0.1),
      labels = percent_format(accuracy = 1),
      na.value = "grey90"
    ) +
    scale_x_discrete(
      labels = percent(target_vals, accuracy = 1),
      expand = c(0, 0)
    ) +
    scale_y_discrete(
      labels = percent(target_vals, accuracy = 1),
      expand = c(0, 0)
    ) +
    labs(
      title = paste("Absolute Impact of Targeting (", title_label, ")", sep = ""),
      subtitle = "Difference in % Reduction of Annualized Disease Incidence (Targeted − Random)",
      x = "Far-UVC Coverage (%)",
      y = "Far-UVC Efficacy (%)",
      fill = "Targeting Benefit"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 11),
      legend.position = "right"
    )
}

# Generate both heatmaps
panel_C_sc2 <- make_heatmap(reductions_summary, "sars_cov_2", "SARS-CoV-2")
panel_C_flu <- make_heatmap(reductions_summary, "flu", "Influenza")

cowplot::plot_grid(panel_C_sc2, panel_C_flu, ncol = 2, labels = c("A", "B"))
# Panel D: Absolute reduction in active infection prevalence vs baseline
# Panel E: 
# Panel F: Relative extra impact in active infection prevalence





# % reduction plotted for random vs targeted
# Panel E: Absolute reduction in prevalence vs baseline (reuses these runs)

# Panel B: Relative extra impact in annualized incidence
# Panel F: Relative extra impact in prevalence (reuses these runs)

# Panel C: Heatmap of absolute impact (incidence, targeted − random)


# Panel D: Heat map of relative extra impact (incidence)
#  same coverage × efficacy grid as Panel C


# Panel E: Flu Active Infections (post period only)


#Panel F: Flu Heatmap

#Combined flu Plots

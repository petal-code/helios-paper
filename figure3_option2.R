library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(viridis)
library(cowplot)
library(helios)


dir_sim <- "/Users/geethaj/documents/helios_files/figure_3_simulations"
files <- list.files(path = dir_sim, pattern = "\\.rds$", full.names = TRUE)

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
      coverage_type = sim$parameters$far_uvc_joint_coverage_type
    ) %>%
    group_by(year, filename, archetype, coverage, efficacy, coverage_type) %>%
    summarise(
      annualized_incidence = sum(E_new, na.rm = TRUE) / N,
      .groups = "drop"
    )
}

all_sims <- map_dfr(files, read_simulation)

metrics <- all_sims %>%
  mutate(window = case_when(
    year %in% 5:7   ~ "baseline",
    year %in% 15:17 ~ "post",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(window)) %>%
  group_by(filename, archetype, coverage, efficacy, coverage_type, window) %>%
  summarise(
    mean_annualized_incidence = mean(annualized_incidence, na.rm = TRUE),
    .groups = "drop"
  )

# % reduction per run
reductions <- metrics %>%
  pivot_wider(
    names_from = window,
    values_from = mean_annualized_incidence,
    names_glue = "mean_annualized_incidence_{window}"
  ) %>%
  mutate(
    pct_reduction_incidence =
      (mean_annualized_incidence_baseline - mean_annualized_incidence_post) /
      mean_annualized_incidence_baseline
  )

# pairing targeted and random indv runs
reductions_paired <- reductions %>%
  group_by(archetype, coverage, efficacy, coverage_type) %>%
  mutate(run_id = row_number()) %>%
  pivot_wider(
    id_cols = c(archetype, coverage, efficacy, run_id),
    names_from = coverage_type,
    values_from = pct_reduction_incidence
  ) %>%
  filter(!is.na(random), !is.na(targeted_riskiness)) %>%
  mutate(
    diff_inc  = targeted_riskiness - random,
    ratio_inc = targeted_riskiness / pmax(random, 1e-9)
  ) %>%
  filter(ratio_inc > 0 & ratio_inc < 9)

summary(reductions_paired$diff_inc)
summary(reductions_paired$ratio_inc)

#Panel B: % reduction in annualized incidence - SC2
panel_b_summary <- reductions %>%
  filter(archetype == "sars_cov_2",
         efficacy %in% c(0.4, 0.6, 0.8),
         coverage %in% c(0.2, 0.4, 0.6, 0.8)) %>%
  group_by(efficacy, coverage, coverage_type) %>%
  summarise(
    mean_reduction = mean(pct_reduction_incidence, na.rm = TRUE),
    low = quantile(pct_reduction_incidence, 0.05, na.rm = TRUE),
    hi  = quantile(pct_reduction_incidence, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    efficacy_label = paste0("Efficacy = ", percent(efficacy, accuracy = 1)),
    coverage_label = percent(coverage, accuracy = 1)
  )

dodge <- position_dodge(width = 0.8)

panel_b <- ggplot(panel_b_summary,
                  aes(x = factor(coverage_label),
                      y = mean_reduction,
                      fill = coverage_type,
                      group = coverage_type)) +
  geom_col(position = dodge, width = 0.9) +
  geom_errorbar(aes(ymin = low, ymax = hi),
                position = dodge, width = 0.15, color = "black", linewidth = 0.6) +
  facet_wrap(~ efficacy_label, ncol = 1) +
  scale_fill_manual(
    values = c("random" = "#6BAED6", "targeted_riskiness" = "#08519C"),
    labels = c("random" = "Random", "targeted_riskiness" = "Targeted")
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Far-UVC Coverage",
       y = "% Reduction \n (vs baseline)",
       fill = "Deployment Type") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold"))

panel_b


# Panel C:  Relative Extra Impact of Targeted vs Random - SC2

panel_c_summary <- reductions_paired %>%
  filter(archetype =="sars_cov_2",
         efficacy %in% c(0.4, 0.6, 0.8),
         coverage %in% c(0.2, 0.4, 0.6, 0.8)) %>%
  group_by(coverage, efficacy) %>%
  summarise(
    mean_rel_impact = mean(ratio_inc, na.rm = TRUE),
    low  = quantile(ratio_inc, 0.25, na.rm = TRUE),
    high = quantile(ratio_inc, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_label = percent(coverage, accuracy = 1),
    efficacy_label = paste0(percent(efficacy, accuracy = 1), " Efficacy")
  )

panel_c <- ggplot(panel_c_summary,
                  aes(x = factor(coverage_label),
                      y = mean_rel_impact,
                      fill = factor(efficacy))) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = dodge, width = 0.25, color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = c("0.4" = "#92dbaa", "0.6" = "#38a85e", "0.8" = "#246b3c"),
    labels = c("40%", "60%", "80%"),
    name = "Efficacy"
  ) +
  labs(
    x = "Far-UVC Coverage",
    y = "Targeted / Random \n Reduction Ratio",  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

panel_c

#Panel D: Absolute Differences b/w random and Targeted - SC2

panel_d_summary <- reductions_paired %>%
  filter(archetype == "sars_cov_2",
         efficacy %in% c(0.4, 0.6, 0.8),
         coverage %in% c(0.2, 0.4, 0.6, 0.8)) %>%
  group_by(coverage, efficacy) %>%
  summarise(
    mean_diff = mean(diff_inc, na.rm = TRUE),
    low  = quantile(diff_inc, 0.25, na.rm = TRUE),
    high = quantile(diff_inc, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_label = percent(coverage, accuracy = 1),
    efficacy_label = paste0(percent(efficacy, accuracy = 1), " Efficacy")
  )

panel_d <- ggplot(panel_d_summary,
                  aes(x = factor(coverage_label),
                      y = mean_diff,
                      fill = factor(efficacy))) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = dodge, width = 0.25, color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = c("0.4" = "#6BAED6", "0.6" = "#2171B5", "0.8" = "#08306B"),
    labels = c("40%", "60%", "80%"),
    name = "Efficacy"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Far-UVC Coverage",
    y = "Absolute Difference in\n  % Reduction",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

panel_d


#Panel E: % reduction in annualized incidence - flu
panel_e_summary <- reductions %>%
  filter(archetype == "flu",
         efficacy %in% c(0.4, 0.6, 0.8),
         coverage %in% c(0.2, 0.4, 0.6, 0.8)) %>%
  group_by(efficacy, coverage, coverage_type) %>%
  summarise(
    mean_reduction = mean(pct_reduction_incidence, na.rm = TRUE),
    low = quantile(pct_reduction_incidence, 0.05, na.rm = TRUE),
    hi  = quantile(pct_reduction_incidence, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    efficacy_label = paste0("Efficacy = ", percent(efficacy, accuracy = 1)),
    coverage_label = percent(coverage, accuracy = 1)
  )

dodge <- position_dodge(width = 0.8)

panel_e <- ggplot(panel_e_summary,
                  aes(x = factor(coverage_label),
                      y = mean_reduction,
                      fill = coverage_type,
                      group = coverage_type)) +
  geom_col(position = dodge, width = 0.9) +
  geom_errorbar(aes(ymin = low, ymax = hi),
                position = dodge, width = 0.15, color = "black", linewidth = 0.6) +
  facet_wrap(~ efficacy_label, ncol = 1) +
  scale_fill_manual(
    values = c("random" = "#EC98A4", "targeted_riskiness" = "#DB3951"),
    labels = c("random" = "Random", "targeted_riskiness" = "Targeted")
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Far-UVC Coverage",
       y = "% Reduction \n (vs baseline)",
       fill = "Deployment Type") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold"))

panel_e


#Panel F : Ratio of reduction - flu

panel_f_summary <- reductions_paired %>%
  filter(archetype == "flu", 
         efficacy %in% c(0.4, 0.6, 0.8),
         coverage %in% c(0.2, 0.4, 0.6, 0.8)) %>%
  group_by(coverage, efficacy) %>%
  summarise(
    mean_rel_impact = mean(ratio_inc, na.rm = TRUE),
    low  = quantile(ratio_inc, 0.25, na.rm = TRUE),
    high = quantile(ratio_inc, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_label = percent(coverage, accuracy = 1),
    efficacy_label = paste0(percent(efficacy, accuracy = 1), " Efficacy")
  )

panel_f <- ggplot(panel_f_summary,
                  aes(x = factor(coverage_label),
                      y = mean_rel_impact,
                      fill = factor(efficacy))) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = dodge, width = 0.25, color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = c("0.4" = "#DC2ECA", "0.6" = "#77146d", "0.8" = "#460c40"),
    labels = c("40%", "60%", "80%"),
    name = "Efficacy"
  ) +
  labs(
    x = "Far-UVC Coverage",
    y = "Targeted / Random \n Reduction Ratio",  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

panel_f


#Panel G: Absolute Difference - flu
panel_g_summary <- reductions_paired %>%
  filter(archetype == "flu",
         efficacy %in% c(0.4, 0.6, 0.8),
         coverage %in% c(0.2, 0.4, 0.6, 0.8)) %>%
  group_by(coverage, efficacy) %>%
  summarise(
    mean_diff = mean(diff_inc, na.rm = TRUE),
    low  = quantile(diff_inc, 0.25, na.rm = TRUE),
    high = quantile(diff_inc, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage_label = percent(coverage, accuracy = 1),
    efficacy_label = paste0(percent(efficacy, accuracy = 1), " Efficacy")
  )

panel_g <- ggplot(panel_g_summary,
                  aes(x = factor(coverage_label),
                      y = mean_diff,
                      fill = factor(efficacy))) +
  geom_col(position = dodge, width = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = dodge, width = 0.25, color = "black", linewidth = 0.8) +
  scale_fill_manual(
    values = c("0.4" = "#fec9ae", "0.6" = "#fd8549", "0.8" = "#df4b01"),
    labels = c("40%", "60%", "80%"),
    name = "Efficacy"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Far-UVC Coverage",
    y = "Absolute Difference in \n % Reduction",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

panel_g

#Combined Plots
#SC2

right_column <- plot_grid(
  panel_c, 
  panel_d, 
  ncol = 1, 
  labels = c("C", "D") 
)

combined_sc2_plot <- plot_grid(
  panel_b, 
  right_column, 
  ncol = 2,
  labels = c("B", ""),
)

combined_sc2_plot
#Combined Flu
right_column <- plot_grid(
  panel_f, 
  panel_g, 
  ncol = 1, 
  labels = c("F", "G") # Label them here
)

combined_flu_plot <- plot_grid(
  panel_e, 
  right_column, 
  ncol = 2,
  labels = c("E", "") 
)

combined_flu_plot



complete_combined_plot <- plot_grid (combined_sc2_plot, combined_flu_plot, ncol = 2)
complete_combined_plot




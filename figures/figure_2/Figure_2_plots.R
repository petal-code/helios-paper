library(dplyr)
library(purrr)
library(tibble)
library(tidyr)


dir_sim <- "/Users/geethaj/helios-paper/figures/figure_2/figure_2_simulations_20250815_2114"
files <- list.files(dir_sim, pattern = "\\.rds$", full.names = TRUE)

read_one <- function(fp) {
  x <- readRDS(fp)
  if (is.list(x) && !is.null(x$simulation)) {
    out <- x$simulation
    meta <- as_tibble(x[names(x) != "simulation"])
    out <- bind_cols(out, meta[rep(1, nrow(out)), , drop = FALSE])
  } else {
    out <- x
  }
  out$filename <- basename(fp)
  out
}

all_sims <- map_dfr(files, read_one, .id = "sim_id")

param_lists <- readRDS("/Users/geethaj/helios-paper/figures/figure_2/figure_2_parameter_list.rds")

param_df <- map_dfr(
  param_lists,
  ~ tibble(
    simulation_id    = .x$simulation_id,
    archetype_label  = .x$archetype_label,
    panel            = .x$panel,
    coverage         = .x$coverage,
    efficacy         = .x$efficacy,
    coverage_type    = .x$coverage_type,
    iteration_number = .x$iteration_number
  )
) %>%
  mutate(sim_id = as.character(simulation_id))

all_sims_labeled <- all_sims %>%
  left_join(param_df, by = "sim_id")

dt_val <- 0.5   # 0.5 days per timestep
burn_in <- 5 * 365   # 5 years in days

all_sims_burned <- all_sims_labeled %>%
  filter(timestep * dt_val > burn_in)

by_run <- all_sims_burned %>%
  group_by(sim_id, archetype_label, panel, coverage, efficacy, iteration_number) %>%
  summarise(
    steps = n(),
    total_incidence = sum(E_new, na.rm = TRUE),
    mean_prevalence_I  = mean(I_count) / 50000,              # I/N
    mean_prevalence_EI = mean(E_count + I_count) / 50000,    # (E+I)/N
    .groups = "drop"
  ) %>%
  mutate(
    days = steps * dt_val,
    inc_absolute_per_year = (total_incidence / days) * 365
  )

by_group <- by_run %>%
  group_by(archetype_label, panel, coverage, efficacy) %>%
  summarise(
    mean_inc = mean(inc_absolute_per_year, na.rm = TRUE),
    lo_inc   = quantile(inc_absolute_per_year, 0.25, na.rm = TRUE),
    hi_inc   = quantile(inc_absolute_per_year, 0.75, na.rm = TRUE),
    mean_prev_I  = mean(mean_prevalence_I, na.rm = TRUE),
    mean_prev_EI = mean(mean_prevalence_EI, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(by_run)
glimpse(by_group)


#Get Summary Statistics


#Panel A-D: SC2

#Panel A: Annualized disease Incidence across UV-C Efficacy values (Line Graph)
panel_a_plot <- by_group %>%
  filter(panel == "panel_A", coverage == 0.5) %>%
  ggplot(aes(x = efficacy, y = mean_inc, color = archetype_label, group = archetype_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lo_inc, ymax = hi_inc, fill = archetype_label),
              alpha = 0.2, color = NA) +
  facet_wrap(~archetype_label, scales = "free_y") +
  labs(
    title = "Panel A: Annual Incidence by UV-C Efficacy",
    subtitle = "Coverage fixed at 50%",
    x = "UV-C Efficacy",
    y = "Annual Incidence (infections/year)",
    color = "Pathogen",
    fill = "Pathogen"
  ) +
  theme_minimal()

print(panel_a_plot)

#Panel B: % Reduction in Annualized disease Incidence across UV-C Coverage value
panel_b_plot <- by_group %>%
  filter(panel == "panel_B", efficacy == 0.5) %>%
  ggplot(aes(x = coverage, y = mean_inc, color = archetype_label, group = archetype_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lo_inc, ymax = hi_inc, fill = archetype_label),
              alpha = 0.2, color = NA) +
  facet_wrap(~archetype_label, scales = "free_y") +
  labs(
    title = "Panel B: Annual Incidence by UV-C Coverage",
    subtitle = "Efficacy fixed at 50%",
    x = "UV-C Coverage",
    y = "Annual Incidence (infections/year)",
    color = "Pathogen",
    fill = "Pathogen"
  ) +
  theme_minimal()

print(panel_b_plot)

#Panel C: Heatmap, UVC Coverage x UVC Efficacy , % reduction in annualized disease incidence

panel_c_plot <- by_group %>%
  filter(panel == "panel_C") %>%
  ggplot(aes(x = coverage, y = efficacy, fill = mean_inc)) +
  geom_tile(color = "white") +
  facet_wrap(~archetype_label) +
  scale_x_continuous(breaks = seq(0, 1.0, 0.2), labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.2), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_c(option = "plasma", name = "Annual incidence") +
  labs(
    title = "Panel C: Heatmap of Annual Incidence",
    x = "Coverage",
    y = "Efficacy"
  ) +
  theme_minimal()

print(panel_c_plot)

#Panel D:Active Infection Prevalence by UV-C Coverage (Line plot)

#Panel E-H: Flu

#Panel E: Annualized infection Incidence across UV-C Efficacy values (Line Graph)

#Panel F: % Reduction in Annualized infection Incidence across UV-C Coverage value

#Panel G: Heatmap, UVC Coverage x UVC Efficacy , % reduction in annualized disease incidence

#Panel H: Active Infection Prevalence by UV-C Coverage (Line plot)


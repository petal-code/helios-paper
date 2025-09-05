#Panel A-D: SC2

#Panel A: Annualized disease Incidence across UV-C Efficacy values (Line Graph)

#Panel B: % Reduction in Annualized disease Incidence across UV-C Coverage value

#Panel C: Heatmap, UVC Coverage x UVC Efficacy , % reduction in annualized disease incidence

#Panel D:Active Infection Prevalence by UV-C Coverage (Line plot)

#Panel E-H: Flu

#Panel E: Annualized infection Incidence across UV-C Efficacy values (Line Graph)

#Panel F: % Reduction in Annualized infection Incidence across UV-C Coverage value

#Panel G: Heatmap, UVC Coverage x UVC Efficacy , % reduction in annualized disease incidence

#Panel H: Active Infection Prevalence by UV-C Coverage (Line plot)
sim <- figure_2_scenario_endemic_id_1_iteration_1_scenario_20250815_2114_output$simulation

# Inspect it
dim(sim)      # rows x cols
head(sim)     # first few rows
str(sim)      # see column names and types

plot(sim$timestep, sim$I_count, type = "l",
     xlab = "timestep", ylab = "Infectious (I)",
     main = "Infectious over time")

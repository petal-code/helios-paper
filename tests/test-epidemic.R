library("")

download.file(
  url = "https://raw.githubusercontent.com/mrc-ide/helios/main/vignettes/Blueprint_Report2_July24_files/exemplar_simulations.rds",
  destfile = "example_simulations.rds",
  mode = "wb" # important for binary files
)

sims <- readRDS("example_simulations.rds")
str(sims)

time_to_peak(sims)
epidemic_final_size(sims)
peak_incidence(sims)

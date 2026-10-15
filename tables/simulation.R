table_data <- data.frame(
  "Setting" = c(
    "Pathogen archetype",
    "AQI efficacy",
    "AQI coverage",
    "AQI deployment",
    "Transmission regime",
    "Stochastic replicates"
  ),
  "Values" = c(
    "SARS-CoV-2; influenza A/B",
    "20%, 40%, 60%, 80%, 100%",
    "0%, 20%, 40%, 60%, 80%, 100%",
    "Random; Risk-targeted",
    "Endemic; epidemic",
    "25"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

table <- gt::gt(table_data) |>
  gt::cols_align(align = "left", columns = everything())

saveRDS(table, here::here("tables", "simulation.rds"))

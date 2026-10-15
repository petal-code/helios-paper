table_data <- data.frame(
  "Parameter" = c(
    "Approximate basic reproduction number (${R}_{0}$)",
    "Mean incubation period (days)",
    "Mean infectious period (days)",
    "Mean duration of immunity (days)"
  ),
  "SARS-CoV-2" = c(
    "2.5",
    "2",
    "4",
    "365"
  ),
  "Influenza A/B" = c(
    "1.5",
    "1",
    "2",
    "365"
  ),
  "Source(s)" = c(
    "[@liu2020the; @biggerstaff2014estimates]",
    "Model assumption",
    "Model assumption",
    "Model assumption"
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

table <- gt::gt(table_data) |>
  gt::cols_align(align = "left", columns = everything()) |>
  gt::cols_align(align = "center", columns = c(2, 3))

saveRDS(table, here::here("tables", "pathogen.rds"))

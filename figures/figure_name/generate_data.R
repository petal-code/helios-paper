source("packages.R")

path <- file.path("figures", "figure_name")

data <- data.frame(
  x = seq(1, 10, by = 0.1),
  y = sin(seq(1, 10, by = 0.1))
)

write.csv(data, file.path(path, "data.csv"))

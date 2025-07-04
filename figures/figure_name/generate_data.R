source(here::here("packages.R"))

data <- data.frame(
  x = seq(1, 10, by = 0.1),
  y = sin(seq(1, 10, by = 0.1))
)

write.csv(data, "data.csv")

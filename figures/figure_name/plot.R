source("packages.R")

path <- file.path("figures", "figure_name")

data <- read.csv(file.path(path, "data.csv"))

ggplot(data, aes(x = x, y = y)) +
  geom_line()

ggsave(file.path(path, "plot.pdf"))

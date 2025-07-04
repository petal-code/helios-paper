source(here::here("packages.R"))

data <- read.csv("data.csv")

ggplot(data, aes(x = x, y = y)) +
  geom_line()

ggsave("plot.pdf")

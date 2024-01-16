library(tidyverse)

theme_set(theme_minimal())

data <- read_csv("data/nyc_repeater_status.csv") |>
  select(freq, band, dist, bearing, rx) |>
  mutate(rx = rx / 11)  # convert to 0-1 scale

glimpse(data)

## Initial Visuals

data |>
  filter(band == "2m") |>
  ggplot(aes(y = rx, x = freq, size = dist, color = bearing)) +
  geom_point() +
  scale_color_viridis_c(limits = c(0, 360), breaks = c(0, 180, 270, 360))

data |>
  filter(band == "70cm") |>
  ggplot(aes(y = rx, x = freq, size = dist)) +
  geom_point() 

data |>
  group_by()

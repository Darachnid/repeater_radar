library(tidyverse)
library(ggthemes)

data <- read_csv("data/nyc_repeater_status.csv") |>
  select(channel, freq, band, dist, bearing, left_open_bearing,
         right_open_bearing, rx) |>
  mutate(color = case_when(rx == 0 ~ "NULL",
                           band == "2m" ~ "2m",
                           band == "70cm" ~ "70cm",
                           TRUE ~ "purple"),
         size = case_when(rx == 0 ~ 1,
                          TRUE ~ rx))

ggplot(data,
       aes(bearing, dist)) +
  geom_segment(aes(x = 120, y = 0, xend = 120, yend = 25)) +
  geom_segment(aes(x = 300, y = 0, xend = 300, yend = 25)) +
  geom_segment(aes(x = 120, y = 25, xend = 300, yend = 25)) +
  geom_jitter(aes(color = color, size = size), alpha = 0.5) +
  scale_x_continuous(limits = c(0, 360),
                     breaks =  c(0, 90, 180, 270)) +
  scale_color_manual(values = c("NULL" = "grey",
                                "2m" = "orange",
                                "70cm" = "black")) +
  coord_polar(start = pi/0.5, direction = 1) +
  theme_minimal() +
  labs(title = "Signal Strength of NYC Repeaters",
       size = "Signal Strength",
       y = "Distance",
       x = "Bearing",
       color = "Band")

ggsave("output/NYC.png", bg = "white")  

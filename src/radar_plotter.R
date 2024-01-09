library(tidyverse)
library(ggthemes)
library(ggrepel)

########################## NYC ############################

data <- read_csv("data/nyc_repeater_status.csv") |>
  select(channel, freq, band, dist, bearing, left_open_bearing,
         right_open_bearing, rx, callsign) |>
  mutate(color = case_when(rx == 0 ~ "NULL",
                           band == "2m" ~ "2m",
                           band == "70cm" ~ "70cm",
                           TRUE ~ "purple"),
         size = case_when(rx == 0 ~ 1,
                          TRUE ~ rx),
         label = case_when(rx >= 6 ~ callsign,
                           TRUE ~ ""))

ggplot(data,
       aes(bearing, dist, label = label, color = band)) +
  geom_segment(aes(x = 120, y = 0, xend = 120, yend = 25)) +
  geom_segment(aes(x = 300, y = 0, xend = 300, yend = 25)) +
  geom_segment(aes(x = 120, y = 25, xend = 300, yend = 25)) +
  geom_point(aes(color = color, size = size), alpha = 0.5) +
  geom_text_repel(min.segment.length = 0, force = 5, 
                  size = 3, position = "dodge", direction = "both") +
  scale_x_continuous(limits = c(0, 360),
                     breaks =  c(0, 90, 180, 270)) +
  scale_size(range = c(1, 4)) +
  scale_color_manual(values = c("NULL" = "grey",
                                "2m" = "orange",
                                "70cm" = "black")) +
  coord_polar(start = pi/0.5, direction = 1) +
  theme_minimal() +
  ylim(0, 25) +
  labs(title = "Signal Strength of NYC Repeaters",
       size = "Signal Strength",
       y = "Distance",
       x = "Bearing",
       color = "Band")

ggsave("output/nyc_radar.png", bg = "white")  

########################## BOS ############################

data <- read_csv("data/bos_repeater_status.csv") |>
  select(channel, freq, band, dist, bearing, left_open_bearing,
         right_open_bearing, rx, callsign) |>
  mutate(color = case_when(rx == 0 ~ "NULL",
                           band == "2m" ~ "2m",
                           band == "70cm" ~ "70cm",
                           TRUE ~ "purple"),
         size = case_when(rx == 0 ~ 1,
                          TRUE ~ 2),
         label = case_when(rx != 0 ~ callsign,
                           TRUE ~ "")) 

ggplot(data,
       aes(bearing, dist, label = label)) +
  geom_segment(aes(x = 110, y = 0, xend = 110, yend = 16)) +
  geom_segment(aes(x = 330, y = 0, xend = 330, yend = 16)) +
  geom_segment(aes(x = 110, y = 16, xend = 330, yend = 16)) +
  geom_jitter(aes(color = color, size = size), alpha = 0.8) +
  geom_text_repel(min.segment.length = 0, nudge_x = 6, nudge_y = 5, aes(hjust = -0.1, vjust = -3),
                  force = 4) +
  scale_x_continuous(limits = c(0, 360),
                     breaks =  c(0, 90, 180, 270)) +
  scale_color_manual(values = c("NULL" = "grey55",
                                "2m" = "orange",
                                "70cm" = "black")) +
  scale_size(range = c(1, 3)) +
  coord_polar(start = pi/0.5, direction = 1) +
  ylim(0, 16) +
  theme_minimal() +
  labs(title = "Signal Strength of Boston Repeaters",
       size = "Signal Strength",
       y = "Distance",
       x = "Bearing",
       color = "Band")

ggsave("output/bos_radar.png", bg = "white")  


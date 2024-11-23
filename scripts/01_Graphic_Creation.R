# load packages ----
library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(sf)
library(maps)

# load data ----
load("data/team_data.rda")

# tooling around ----
us_map <- maps::map(
  database = "usa",
  plot = FALSE,
  fill = TRUE) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = us_map, color = "black") +
  geom_cfb_logos(data = team_data, aes(x = longitude, y = latitude, team = school),
                 height = 0.05, width = 0.05) +
  theme_void() +
  coord_sf()
# load packages ----
library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(sf)
library(maps)

# load data ----
load("data/team_data.rda")
load("data/player_data.rda")

team_data <- team_data %>%
  mutate(
    temp = if_else(color == "#ffffff", alt_color, color),
    alt_color = if_else(color == "#ffffff", color, alt_color),
    color = temp) %>%
  select(-temp)

# top team bar chart ----
team_data %>%
  slice_max(order_by = `Total Yards`, n = 10) %>%
  ggplot(aes(x = reorder(school, -`Total Yards`), y = `Total Yards`)) +
  geom_col(aes(fill = color, color = alt_color), alpha = 0.85) +
  geom_from_path(aes(path = logo), height = 0.1) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = NULL)

# team scatter plot ----
team_data %>%
  ggplot(aes(x = `Rush Attempts`, y = `Rushing Yards`)) +
  geom_hline(yintercept = mean(team_data$`Rushing Yards`), lty = 2, color = "grey") +
  geom_vline(xintercept = mean(team_data$`Rush Attempts`), lty = 2, color = "grey") +
  geom_from_path(aes(path = logo), height = 0.05)

# top player bar chart ----
player_data %>%
  slice_max(order_by = `Rushing TDs`, n = 10) %>%
  ggplot(aes(x = reorder(player, -`Rushing TDs`), y = `Rushing TDs`)) +
  geom_col(aes(fill = color, color = alt_color), alpha = 0.85) +
  geom_from_path(aes(path = logo), height = 0.1) +
  geom_from_path(aes(path = headshot_url, y = 1.5), height = 0.1) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = NULL) +
  theme_minimal()

# tooling around ----
us_map <- map_data(map = "state") %>%
  mutate(state = state.abb[match(region, tolower(state.name))]) %>%
  select(long, lat, group, state)

team_data %>%
  summarize(avg_off = mean(`Total Yards`), .by = state) %>%
  mutate(avg_off = floor((rank(avg_off) - 1) / (n() - 1) * 98) + 1) %>%
  right_join(us_map, by = join_by(state == state)) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = avg_off), color = "black") + 
  coord_quickmap() +
  theme_void() +
  scale_fill_gradient2(low = "darkred", high = "darkgreen", midpoint = 50, mid = "white", na.value = "grey") +
  theme(legend.position = "none")

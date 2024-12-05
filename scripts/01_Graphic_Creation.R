# load packages ----
library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(sf)
library(maps)
library(gt)
library(gtExtras)
library(glue)
library(htmltools)
library(ggthemes)

# load data ----
load("data/team_data.rda")
load("data/player_data.rda")

team_data <- team_data %>%
  mutate(
    temp = if_else(color == "#ffffff", alt_color, color),
    alt_color = if_else(color == "#ffffff", color, alt_color),
    color = temp) %>%
  select(-temp)

# team charts ----
## top team bar chart ----
team_data %>%
  slice_max(order_by = `Total Yards`, n = 10) %>%
  ggplot(aes(x = reorder(school, -`Total Yards`), y = `Total Yards`)) +
  geom_col(aes(fill = color, color = alt_color), alpha = 0.85) +
  geom_from_path(aes(path = logo), height = 0.1) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = NULL)

## team scatter plot ----
team_data %>%
  ggplot(aes(x = `Rush Attempts`, y = `Rushing Yards`)) +
  geom_hline(yintercept = mean(team_data$`Rushing Yards`), lty = 2, color = "grey") +
  geom_vline(xintercept = mean(team_data$`Rush Attempts`), lty = 2, color = "grey") +
  geom_from_path(aes(path = logo), height = 0.05)

## team radial bar chart ----
percent_labels <- tibble(
  x = rep(0.5, 4),
  y = c(12.5, 37.5, 62.5, 87.5),
  label = c("1st-25th", "25th-50th", "50th-75th", "75th-99th"))

team_data %>%
  mutate(pass_yd_percentile = floor((rank(`Passing Yards`) - 1) / (n() - 1) * 98) + 1,
         pass_td_percentile = floor((rank(`Passing TDs`) - 1) / (n() - 1) * 98) + 1,
         pass_ypa_percentile = floor((rank(`Passing Yards` / `Pass Attempts`) - 1) / (n() - 1) * 98) + 1,
         rush_yd_percentile = floor((rank(`Rushing Yards`) - 1) / (n() - 1) * 98) + 1,
         rush_td_percentile = floor((rank(`Rushing TDs`) - 1) / (n() - 1) * 98) + 1,
         rush_ypa_percentile = floor((rank(`Rushing Yards` / `Rush Attempts`) - 1) / (n() - 1) * 98) + 1,
         ) %>%
  select(school, color, logo, alt_color, ends_with("percentile")) %>%
  filter(school == "Alabama") %>%
  pivot_longer(cols = ends_with("percentile"), names_to = "category") %>%
  ggplot(aes(x = category, y = value)) +
  geom_col(aes(fill = color, group = school), width = 1) +
  geom_hline(yintercept = c(25, 50, 75, 99), color = "black", lty = 3) +
  geom_segment(x = 0.5:5.5, y = 0, xend = 0.5:5.5, yend = 99, color = "black") +
  annotate(geom = "text",
           label = c("Passing\nYards", "Passing\nTDs", "Passing\nEfficiency",
                     "Rushing\nEfficiency", "Rushing\nTDs", "Rushing\nYards"),
           x = seq(1:6), y = 120, size = 4) +
  geom_label(data = percent_labels, aes(x = x, y = y, label = label)) +
  coord_polar() +
  scale_x_discrete(limits = c("pass_yd_percentile", "pass_td_percentile",
    "pass_ypa_percentile", "rush_ypa_percentile", "rush_td_percentile", "rush_yd_percentile"),
    expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void()

## team stats table ----
team_data %>%
  mutate(`Completion Percentage` = str_c(round(100 * Completions / `Pass Attempts`, digits = 1), "%")) %>%
  filter(school == "Air Force") %>%
  select(`Passing Yards`, `Passing TDs`, `Completion Percentage`,
         `Rushing Yards`, `Rushing TDs`, Turnovers, `3rd Down Conversion`, `4th Down Conversion`) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "stat") %>%
  mutate(category = case_when(stat %in% c("Passing Yards", "Passing TDs", "Completion Percentage") ~ "In the Air",
                              stat %in% c("Rushing Yards", "Rushing TDs") ~ "On the Ground", .default = "Miscellaneous"),
         `FBS Rank` = 1) %>%
  gt(groupname_col = "category") %>%
  tab_header(title = "Alabama Crimson Tide", subtitle = "Through 12/5/2024") %>%
  tab_style(locations = cells_title(), cell_fill("#9e1632")) %>%
  tab_style(locations = cells_title(), cell_text(color = "white", weight = "bold"))

# player charts ----
## top player bar chart ----
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

# percentile donut chart ----
player_data %>%
  filter(position == "QB") %>%
  mutate(percentile = floor((rank(`Passing Yards`) - 1) / (n() - 1) * 98) + 1) %>%
  filter(player == "Cade Klubnik") %>%
  ggplot(aes(x = 2)) +
  geom_bar(aes(y = -percentile, fill = percentile), stat = "identity", width = 1) +
  geom_bar(aes(y = -(100 - percentile)), fill = "lightgrey", stat = "identity", width = 1) +
  geom_label(aes(x = 0.5, y = 0), label = "Passing Yards", size = 6) +
  scale_fill_gradientn(colors = c("red2", "yellow2", "green2"),
                       values = c(0, 0.6, 1),
                       limits = c(1, 99)) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  theme_void() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

# player table ----
player_data %>%
  filter(player == "Will Rogers") %>%
  mutate(info = str_c(player, " ", position, " #", jersey)) %>%
  select(headshot_url, info) %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = headshot_url),
    fn = function(x) {
      glue(
        '<div style="position: relative; width: 220px; height: 160px;">
           <img src="{player_data$logo[which(player_data$headshot_url == x)]}" 
                style="position: absolute; width: 220px; height: 160px; opacity: 0.2;">
           <img src="{player_data$headshot_url[which(player_data$headshot_url == x)]}" 
                style="position: absolute; width: 220px; height: 160px;">
         </div>'
      )}) %>%
  cols_label(info = "", headshot_url = "") %>%
  tab_options(
    table.width = "300px",
    table.font.size = "12px") %>%
  tab_style(
    style = list(
      cell_text(size = px(24), weight = "bold", align = "center")),
    locations = cells_body(columns = info)) %>%
  cols_width(headshot_url ~ px(260), info ~ px(150))
    
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

# load packages ----
library(tidyverse)
library(cfbfastR)
library(cfbplotR)

# load data ----
player_stats <- cfbd_stats_season_player(year = 2024)
player_data <- load_cfb_rosters(seasons = 2024)
team_stats <- cfbd_stats_season_team(year = 2024)
team_data <- load_cfb_teams(fbs_only = TRUE)

game_stats <- rbind(
  cfbd_game_player_stats(year = 2024, week = 1) %>% mutate(week = 1),
  cfbd_game_player_stats(year = 2024, week = 2) %>% mutate(week = 2),
  cfbd_game_player_stats(year = 2024, week = 3) %>% mutate(week = 3),
  cfbd_game_player_stats(year = 2024, week = 4) %>% mutate(week = 4),
  cfbd_game_player_stats(year = 2024, week = 5) %>% mutate(week = 5),
  cfbd_game_player_stats(year = 2024, week = 6) %>% mutate(week = 6),
  cfbd_game_player_stats(year = 2024, week = 7) %>% mutate(week = 7),
  cfbd_game_player_stats(year = 2024, week = 8) %>% mutate(week = 8),
  cfbd_game_player_stats(year = 2024, week = 9) %>% mutate(week = 9),
  cfbd_game_player_stats(year = 2024, week = 10) %>% mutate(week = 10),
  cfbd_game_player_stats(year = 2024, week = 11) %>% mutate(week = 11),
  cfbd_game_player_stats(year = 2024, week = 12) %>% mutate(week = 12),
  cfbd_game_player_stats(year = 2024, week = 13) %>% mutate(week = 13),
  cfbd_game_player_stats(year = 2024, week = 14) %>% mutate(week = 14),
  cfbd_game_player_stats(year = 2024, week = 15) %>% mutate(week = 15))

# variable selection ----
player_stats <- player_stats %>%
  select(athlete_id, player, starts_with("passing"), starts_with("rushing"), starts_with("receiving"))

player_data <- player_data %>%
  select(athlete_id, team, weight, height, jersey, position, headshot_url)

team_stats <- team_stats %>%
  select(-c(season, conference, time_of_poss_total, starts_with("kick"), starts_with("punt"),
            passes_intercepted, passes_intercepted_yds, passes_intercepted_TDs, first_downs,
            penalties, penalty_yds, interceptions, fumbles_lost))


team_data <- team_data %>%
  filter(classification == "fbs") %>%
  select(school, mascot, abbreviation, conference,
         color, alt_color, logo, state, latitude, longitude)

game_stats <- game_stats %>%
  select(team, conference, home_away, week, athlete_name, passing_completions, passing_attempts, passing_yds,
         passing_td, passing_qbr, rushing_car, rushing_yds, rushing_avg, rushing_td, receiving_rec,
         receiving_yds, receiving_avg, receiving_td)

# data cleaning ----
team_stats <- team_stats %>%
  mutate(third_down_pct = round(third_down_convs / third_downs * 100, digits = 1),
         fourth_down_pct = round(fourth_down_convs / fourth_downs * 100, digits = 1),
         team = case_when(team == "App State" ~ "Appalachian State",
                          team == "Massachusetts" ~ "UMass", .default = team)) %>%
  select(-c(third_downs, third_down_convs, fourth_down_convs)) %>%
  rename(Completions = pass_comps, `Pass Attempts` = pass_atts,
         `Passing Yards` = net_pass_yds, `Passing TDs` = pass_TDs,
         `Rush Attempts` = rush_atts, `Rushing Yards` = rush_yds,
         `Rushing TDs` = rush_TDs, `Total Yards` = total_yds,
         `Turnovers` = turnovers, `3rd Down Conversion` = third_down_pct,
         `4th Down Conversion` = fourth_down_pct, `Games Played` = games)
         
team_data <- team_data %>%
  mutate(alt_color = if_else(school == "South Alabama", "#FFFFFF", alt_color),
                school = case_when(school == "Connecticut" ~ "UConn",
                                   school == "Louisiana Monroe" ~ "UL Monroe",
                                   school == "Sam Houston State" ~ "Sam Houston",
                                   school == "Southern Mississippi" ~ "Southern Miss",
                                   school == "UT San Antonio" ~ "UTSA", .default = school),
         full_name = str_c(school, " ", mascot))

fbs_teams <- unique(team_data$school)
positions <- c("QB", "RB", "FB", "WR", "TE")

player_stats <- player_stats %>%
  filter(player != " Team") %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(passing_pct = round(passing_pct * 100, digits = 1)) %>%
  rename(Completions = passing_completions, `Pass Attempts` = passing_att,
         `Completion Percentage` = passing_pct, `Passing Yards` = passing_yds,
         `Passing TDs` = passing_td, Interceptions = passing_int,
         `Yards Per Attempt` = passing_ypa, Carries = rushing_car,
         `Rushing Yards` = rushing_yds, `Rushing TDs` = rushing_td,
         `Yards Per Carry` = rushing_ypc, `Longest Run` = rushing_long,
         `Receptions` = receiving_rec, `Receiving Yards` = receiving_yds,
         `Receiving TDs` = receiving_td, `Yards Per Catch` = receiving_ypr,
         `Longest Catch` = receiving_long)

player_data <- player_data %>%
  mutate(team = case_when(team == "Connecticut" ~ "UConn",
                          team == "Louisiana Monroe" ~ "UL Monroe",
                          team == "Sam Houston State" ~ "Sam Houston",
                          team == "Southern Mississippi" ~ "Southern Miss",
                          team == "UT San Antonio" ~ "UTSA", .default = team),
         feet = floor(height / 12), inches = height %% 12,
         height = str_c(feet, "'", inches),
         weight = str_c(weight, " lbs")) %>%
  filter(team %in% fbs_teams, position %in% positions) %>%
  select(-c(feet, inches))

game_stats <- game_stats %>%
  filter(team %in% fbs_teams) %>%
  mutate(completion_percentage = round(passing_completions / passing_attempts * 100, digits = 1))

# data configuration ----
team_player_data <- team_data %>%
  select(school, conference, color, alt_color, logo, full_name)

player_data <- player_data %>%
  left_join(player_stats, by = join_by(athlete_id == athlete_id)) %>%
  left_join(team_player_data, by = join_by(team == school)) %>%
  relocate(c(player, position), .before = team) %>%
  mutate(jersey = if_else(is.na(jersey), 0, jersey)) %>% # package has issue reading #0 jersey
  filter(!is.na(player), complete.cases(.)) %>% # unfortunately espn doesn't have all information
  select(-athlete_id)

team_data <- team_data %>%
  left_join(team_stats, by = join_by(school == team)) %>%
  relocate(full_name, .after = mascot) %>% relocate(logo, .before = `Games Played`)

game_stats <- game_stats %>%
  left_join(player_data %>% select(player, position, team, color),
            by = join_by(team == team, athlete_name == player)) %>%
  filter(position %in% positions) %>%
  select(athlete_name, team, conference, home_away, week,
         passing_yds, passing_td, completion_percentage, passing_qbr,
         rushing_car, rushing_yds, rushing_td, rushing_avg,
         receiving_rec, receiving_yds, receiving_td, receiving_avg, color) %>% # to reorder manually
  rename(player = athlete_name,
         
         "Passing Yards" = passing_yds,
         "Passing Touchdowns" = passing_td,
         "Completion Percentage" = completion_percentage,
         "Quarterback Rating" = passing_qbr,
         
         "Carries" = rushing_car,
         "Rushing Yards" = rushing_yds,
         "Rushing Touchdowns" = rushing_td,
         "Yards per Carry" = rushing_avg,
         
         "Receptions" = receiving_rec,
         "Receiving Yards" = receiving_yds,
         "Receiving Touchdowns" = receiving_td,
         "Yards per Catch" = receiving_avg)

# save out files ----
save(player_data, file = "thelowdown/data/player_data.rda")
save(team_data, file = "thelowdown/data/team_data.rda")
save(game_stats, file = "thelowdown/data/game_stats.rda")
  
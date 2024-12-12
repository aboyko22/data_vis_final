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

games <- espn_cfb_schedule(year = 2024, season_type = "both", groups = "FBS") %>%
  filter(season == 2024, game_date < Sys.Date()) %>%
  select(game_id, game_date, home_team_location, away_team_location)

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
  select(team, conference, home_away, game_id, week, athlete_name, passing_completions, passing_attempts, passing_yds,
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
  mutate(school = case_when(school == "Connecticut" ~ "UConn",
                             school == "Louisiana Monroe" ~ "UL Monroe",
                             school == "Sam Houston State" ~ "Sam Houston",
                             school == "Southern Mississippi" ~ "Southern Miss",
                             school == "UT San Antonio" ~ "UTSA", .default = school),
         
          color = case_when(school == "North Texas" ~ "#00853E",
                           school == "Charlotte" ~ "#046A38", .default = color),
         
         alt_color = case_when(school == "South Alabama" ~ "#FFFFFF",
                               school == "Bowling Green" ~ "#FE5000",
                               school == "Cincinnati" ~ "#FFFFFF",
                               school == "Marshall" ~ "#A2AAAD",
                               school == "New Mexico" ~ "#BA0C2F",
                               school == "Northern Illinois" ~ "#000000",
                               school == "Penn State" ~ "#FFFFFF",
                               school == "Pittsburgh" ~ "#FFB81C",
                               school == "Southern Miss" ~ "#000000",
                               school == "Vanderbilt" ~ "#866d4b", .default = alt_color),
         full_name = str_c(school, " ", mascot),
         full_name = if_else(school == "Florida International", "FIU Golden Panthers", full_name))

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

games <- games %>%
  mutate(home_team_location = case_when(home_team_location == "App State" ~ "Appalachian State",
                                        home_team_location == "Massachusetts" ~ "UMass", .default = home_team_location),
         away_team_location = case_when(away_team_location == "App State" ~ "Appalachian State",
                                        away_team_location == "Massachusetts" ~ "UMass", .default = away_team_location),
         game_id = as.integer(game_id))

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
  relocate(full_name, .after = mascot) %>% relocate(logo, .before = `Games Played`) # want per game stats?

game_stats <- game_stats %>%
  left_join(player_data %>% select(player, position, team, color),
            by = join_by(team == team, athlete_name == player)) %>%
  left_join(games, by = join_by(game_id == game_id)) %>%
  mutate(game = if_else(team == home_team_location, str_c("Week ", week, " vs.\n", away_team_location),
                            str_c("Week ", week, " at\n", home_team_location))) %>%
  filter(position %in% positions) %>%
  select(athlete_name, team, conference, home_away, game, game_date,
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
         "Yards per Catch" = receiving_avg) %>%
  arrange(game_date, team, player)

# Function to calculate percentiles for a qualified subset
calculate_percentile <- function(values, condition) {
  
  stopifnot(length(values) == length(condition))
  qualified_indices <- which(condition)
  
  ranks <- rank(values[qualified_indices])
  total_valid <- length(qualified_indices)
  
  result <- rep(NA_real_, length(values))
  result[qualified_indices] <- floor((ranks - 1) / (total_valid - 1) * 98) + 1
  
  return(result)
}

percentile_data <- player_data %>%
  mutate(type = case_when(position == "QB" ~ "pass",
                          position %in% c("FB", "RB") ~ "run",
                          position %in% c("WR", "TE") ~ "catch")) %>%
  filter(`Pass Attempts` >= 20 | Carries >= 20 | Receptions >= 20) %>%
  mutate("Passing\nYards" = calculate_percentile(`Passing Yards`, `Pass Attempts` >= 20),
         "Passing\nTDs" = calculate_percentile(`Passing TDs`, `Pass Attempts` >= 20),
         "Completion\nPercentage" = calculate_percentile(`Completion Percentage`, `Pass Attempts` >= 20),
         "Rushing\nYards" = calculate_percentile(`Rushing Yards`, Carries >= 20),
         "Rushing\nTDs" = calculate_percentile(`Rushing TDs`, Carries >= 20),
         "Rushing\nYPC" = calculate_percentile(`Yards Per Carry`, Carries >= 20),
         "Receiving\nYards" = calculate_percentile(`Receiving Yards`, Receptions >= 10),
         "Receiving\nTds" = calculate_percentile(`Receiving TDs`, Receptions >= 10),
         "Receiving\nYPC" = calculate_percentile(`Yards Per Catch`, Receptions >= 10)) %>%
  pivot_longer(cols = c("Passing\nYards", "Passing\nTDs", "Completion\nPercentage",
                        "Rushing\nYards", "Rushing\nTDs", "Rushing\nYPC",
                        "Receiving\nYards", "Receiving\nTds", "Receiving\nYPC"), names_to = "percentile") %>%
  mutate(label = case_when(
    value %% 10 == 1 & value %% 100 != 11 ~ str_c(value, "st"),
    value %% 10 == 2 & value %% 100 != 12 ~ str_c(value, "nd"),
    value %% 10 == 3 & value %% 100 != 13 ~ str_c(value, "rd"),
    TRUE ~ str_c(value, "th"))) %>%
  select(player, type, percentile, value, label)

# save out files ----
save(player_data, file = "thelowdown/data/player_data.rda")
save(team_data, file = "thelowdown/data/team_data.rda")
save(game_stats, file = "thelowdown/data/game_stats.rda")
save(percentile_data, file = "thelowdown/data/percentile_data.rda")
  
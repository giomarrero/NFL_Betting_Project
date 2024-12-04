#install.packages("nflfastR")
#install.packages("nflreadr")
#install.packages(dplyr)
install.packages("rlang")
library(nflreadr)
library(rlang)
library(dplyr)

#Import Data
pbp <- load_pbp(2013:2022)
sched <- load_schedules(2013:2022)

sched <- sched[sched$game_type=="REG",] 
sched

#Change Names for Consistency
sched$home_team[sched$home_team == "STL"] <- "LA"
sched$home_team[sched$home_team == "SD"] <- "LAC"
sched$home_team[sched$home_team == "OAK"] <- "LV"
sched$away_team[sched$away_team == "STL"] <- "LA"
sched$away_team[sched$away_team == "SD"] <- "LAC"
sched$away_team[sched$away_team == "OAK"] <- "LV"

sched <- sched %>%
  select("game_id",  "gameday", "season", "week",  "home_team", "home_score", "away_team", "away_score")


#Winning Margin
#Grab All Home Games
sched_home <- sched %>%
  select("game_id",  "gameday", "season", "week",  "home_team", "home_score", "away_team", "away_score") %>%
  mutate(team = home_team) %>%
  arrange(season, team, week) %>%
  select("game_id", "gameday", "season", "week", "team",  "home_team", "home_score", "away_team", "away_score")

#Grab All Away Games
sched_away <- sched %>%
  select("game_id",  "gameday", "season", "week",  "home_team", "home_score", "away_team", "away_score") %>%
  mutate(team = away_team) %>%
  arrange(season, team, week)  %>%
  select("game_id",  "gameday", "season", "week", "team",  "home_team", "home_score", "away_team", "away_score")

#Combine the Data Frames
team_sched <- rbind(sched_home, sched_away)

#Sort by Posteam and Week
team_sched <- team_sched %>%
  mutate(opponent = ifelse(team == home_team, away_team, home_team),
         team_score = ifelse(team == home_team, home_score, away_score),
         opponent_score = ifelse(team == home_team, away_score, home_score),
         differential = team_score - opponent_score) %>%
  select("game_id", "gameday", "season", "week", "team",  "team_score", "opponent", "opponent_score",
         "differential", "home_team", "away_team") %>%
  arrange(season, team, week)

#Create Variables for Winning Team, and Binary Win Variable
team_sched <- team_sched %>%
  mutate(winning_team = if_else(differential > 0, team, if_else(differential < 0, opponent, NA_character_)),
         losing_team = if_else(differential < 0, team, if_else(differential > 0, opponent, NA_character_)),
         win = case_when(team == winning_team ~ 1,
                         team == losing_team ~ 0,
                         TRUE ~ 0.5),
         loss = 1 - win)

#Lag Variables
test <- team_sched %>%
  group_by(season, team) %>%
  mutate(total_wins = lag(cumsum(win), n = 1),
         total_losses = lag(cumsum(loss), n = 1),
         team_win_pct = total_wins/(total_wins+total_losses),
         won_last_week = lag(win, n=1))

test
head(test)

new_test <- test %>%
  mutate(
    home_team_total_wins = ifelse(home_team == team, total_wins, NA),
    away_team_total_wins = ifelse(away_team == team, total_wins, NA),
    home_team_total_losses = ifelse(home_team == team, total_losses, NA),
    away_team_total_losses = ifelse(away_team == team, total_losses, NA),
    home_team_win_pct = ifelse(home_team == team, team_win_pct, NA),
    away_team_win_pct = ifelse(away_team == team, team_win_pct, NA),
    home_team_won_last_week = ifelse(home_team == team, won_last_week, NA),
    away_team_won_last_week = ifelse(away_team == team, won_last_week, NA)
  )

new_test

home_data <- new_test[new_test$home_team == team, ]

# Create away_data by selecting rows where "away_team" matches the specified team
away_data <- new_test[new_test$away_team == team, ]

#combined_data <- new_test %>%
#  full_join(new_test, by = "game_id") %>%
#  group_by(game_id) %>%
#  summarise(
#    home_team_total_wins = ifelse(all(!is.na(home_team_total_wins)), first(home_team_total_wins), last(home_team_total_wins)),
#    away_team_total_wins = ifelse(all(!is.na(away_team_total_wins)), first(away_team_total_wins), last(away_team_total_wins)),
#    home_team_total_losses = ifelse(all(!is.na(home_team_total_losses)), first(home_team_total_losses), last(home_team_total_losses)),
#    away_team_total_losses = ifelse(all(!is.na(away_team_total_losses)), first(away_team_total_losses), last(away_team_total_losses)),
#    home_team_win_pct = ifelse(all(!is.na(home_team_win_pct)), first(home_team_win_pct), last(home_team_win_pct)),
#    away_team_win_pct = ifelse(all(!is.na(away_team_win_pct)), first(away_team_win_pct), last(away_team_win_pct)),
#    home_team_won_last_week = ifelse(all(!is.na(home_team_won_last_week)), first(home_team_won_last_week), last(home_team_won_last_week)),
#    away_team_won_last_week = ifelse(all(!is.na(away_team_won_last_week)), first(away_team_won_last_week), last(away_team_won_last_week))
#  )

#combined_data


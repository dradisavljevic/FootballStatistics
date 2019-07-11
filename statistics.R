## SET UP ENVIRONMENT AND LOAD DATA -------------------------------------------
# Set working directory to the current files directory ------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading in the required packages --------------------------------------------

install.packages('tidyr')
install.packages('lubridate')
install.packages('magrittr')
install.packages('tidyquant')
install.packages('purrr')
install.packages('dplyr')
install.packages('moments')
install.packages('ggplot2')
install.packages('ggjoy')
install.packages('nortest')
suppressWarnings(suppressPackageStartupMessages({
  library(tidyr)
  library(lubridate)
  library(magrittr)
  library(tidyquant)
  library(purrr)
  library(dplyr)
  library(stringr)
  library(moments)
  library(ggplot2)
  library(ggjoy)
  library(nortest)
}))

# Load functions for data manipulation ----------------------------------------

source('prepare_data.R')
source('descriptive.R')
source('match_result_stat.R')
source('opening_weekend.R')

# Load the data file ----------------------------------------------------------

serbia_csv <- list.files(path = 'SRB', full.names = TRUE)
montengro_csv <- list.files(path = 'MNE', full.names = TRUE)

# Map the data from file ------------------------------------------------------

serbia_raw_data <- map(serbia_csv, read.csv)
montenegro_raw_data <- map(montengro_csv, read.csv)

# Convert data to tibble ------------------------------------------------------

serbia_data <- do.call(rbind, serbia_raw_data) %>% as_tibble()
montenegro_data <- do.call(rbind, montenegro_raw_data) %>% as_tibble()

## PREPARE DATA ---------------------------------------------------------------
# Variable for new dataframe names --------------------------------------------

simple_names <- c('League',
                  'Level',
                  'Season',
                  'Matchday',
                  'Date',
                  'Time',
                  'HostID',
                  'Host',
                  'HostCity',
                  'HostURL',
                  'GuestID',
                  'Guest',
                  'GuestCity',
                  'GuestURL',
                  'HomeGoals',
                  'AwayGoals',
                  'HomeGoalsHalf',
                  'AwayGoalsHalf',
                  'Outcome')

# Replace names row -----------------------------------------------------------

names(serbia_data) <- simple_names
names(montenegro_data) <- simple_names

# Convert date from String to Date type ---------------------------------------

serbia_data$Date <- str_to_date(serbia_data$Date)
montenegro_data$Date <- str_to_date(montenegro_data$Date)

# Spell the cities and club names grammarly correctly -------------------------

serbia_data$HostCity <- sapply(serbia_data$HostCity,
                               capitalize_words)

serbia_data$Host <- sapply(serbia_data$Host,
                               capitalize_words)

montenegro_data$Host <- sapply(montenegro_data$Host,
                                   capitalize_words)

montenegro_data$HostCity <- sapply(montenegro_data$HostCity,
                                   capitalize_words)

# Merge city columns with club name ones --------------------------------------

serbia_data$Host <- str_split_fixed(serbia_data$Host, '\\(', 2)[,1]
montenegro_data$Host <- str_split_fixed(montenegro_data$Host, '\\(', 2)[,1]
serbia_data$Guest <- str_split_fixed(serbia_data$Guest, '\\(', 2)[,1]
montenegro_data$Guest <- str_split_fixed(montenegro_data$Guest, '\\(', 2)[,1]

serbia_data <- merge_name_and_city(serbia_data)
montenegro_data <- merge_name_and_city(montenegro_data)

# Remove whitespace from club name and city columns ---------------------------

serbia_data <- remove_white_space(serbia_data)
montenegro_data <- remove_white_space(montenegro_data)

# Take the end year of a season as season year --------------------------------

serbia_data <- round_up_season(serbia_data)
montenegro_data <- round_up_season(montenegro_data)

# Remove unnecessary columns ----=----------------------------------------------

serbia_data <- select(serbia_data,
                      League,
                      Level,
                      Season,
                      Matchday,
                      Date,
                      Time,
                      Host,
                      Guest,
                      HomeGoals,
                      AwayGoals,
                      Outcome)

montenegro_data <- select(montenegro_data, 
                          League,
                          Level,
                          Season,
                          Matchday,
                          Date,
                          Time,
                          Host,
                          Guest,
                          HomeGoals,
                          AwayGoals,
                          Outcome)

# Check for not defined values ------------------------------------------------

serbia_data %>% 
  summarise_all(function(x) sum(is.na(x)))

montenegro_data %>% 
  summarise_all(function(x) sum(is.na(x)))

# Correct incorrect Date values -----------------------------------------------

incorrect_date_level <- serbia_data$Level[!is.na(serbia_data$Date) & 
                                            year(serbia_data$Date)==2000]
incorrect_date_matchday <- serbia_data$Matchday[!is.na(serbia_data$Date) & 
                                                  year(serbia_data$Date)==2000]
incorrect_date_season <- serbia_data$Season[!is.na(serbia_data$Date) & 
                                              year(serbia_data$Date)==2000]

serbia_data$Date[year(serbia_data$Date)==2000] <- serbia_data$Date[year(serbia_data$Date)!=2000
                 & serbia_data$Level == incorrect_date_level
                 & serbia_data$Matchday == incorrect_date_matchday
                 & serbia_data$Season == incorrect_date_season][1]

# View Data -------------------------------------------------------------------

View(serbia_data, title='Serbia Football Data')
View(montenegro_data,  title='Montenegro Football Data')

## CONDUCT STATISTICAL ANALYSIS ------------------------------------------------
# Get descriptive statistics --------------------------------------------------

get_teams_per_level(serbia_data)
get_teams_per_level(montenegro_data)
get_number_of_clubs_per_city(serbia_data)
get_number_of_clubs_per_city(montenegro_data)

get_home_goals_descriptive(serbia_data)
get_home_goals_descriptive(montenegro_data)
get_away_goals_descriptive(serbia_data)
get_away_goals_descriptive(montenegro_data)
get_combined_goals_descriptive(serbia_data)
get_combined_goals_descriptive(montenegro_data)

get_goal_max_per_matchday(serbia_data)
get_goal_max_per_matchday(montenegro_data)
get_goal_min_per_matchday(serbia_data)
get_goal_min_per_matchday(montenegro_data)

get_win_percentage_league(serbia_data)
get_win_percentage_league(montenegro_data)
get_win_percentage_league_matchday(serbia_data[serbia_data$Level==1,])
get_win_percentage_league_matchday(montenegro_data[montenegro_data$Level==1,])

get_min_max_goals_team(serbia_data)
get_min_max_goals_team(montenegro_data)

get_win_percentage_weekdays(serbia_data)
get_win_percentage_weekdays(montenegro_data)

get_home_goals_descriptive_weekday(serbia_data)
get_home_goals_descriptive_weekday(montenegro_data)
get_away_goals_descriptive_weekday(serbia_data)
get_away_goals_descriptive_weekday(montenegro_data)
get_combined_goals_descriptive_weekday(serbia_data)
get_combined_goals_descriptive_weekday(montenegro_data)

# Use data for seasonal analysis ----------------------------------------------

serbia_data_seasonal <- serbia_data
montenegro_data_seasonal <- montenegro_data

# Default missing resaults to 0:0 as to not affect Goal difference too much ---

montenegro_data_seasonal$HomeGoals[is.na(montenegro_data_seasonal$HomeGoals)] <- 0
montenegro_data_seasonal$AwayGoals[is.na(montenegro_data_seasonal$AwayGoals)] <- 0
serbia_data_seasonal$HomeGoals[is.na(serbia_data_seasonal$HomeGoals)] <- 0
serbia_data_seasonal$AwayGoals[is.na(serbia_data_seasonal$AwayGoals)] <- 0

# Replace missing Date values -------------------------------------------------

serbia_data_seasonal <- replace_missing_dates(serbia_data_seasonal)
montenegro_data_seasonal <- replace_missing_dates(montenegro_data_seasonal)

# Get seasonal data -----------------------------------------------------------

serbia_data_tidy <- seasonal_result_data(serbia_data_seasonal[serbia_data_seasonal$Level==1,])
montenegro_data_tidy <- seasonal_result_data(montenegro_data_seasonal[montenegro_data_seasonal$Level==1,])

# Get seasonal performance per club -------------------------------------------

get_club_seasonal_performance(serbia_data_tidy, 'Partizan (Beograd)')
get_club_seasonal_performance(serbia_data_tidy, 'Crvena Zvezda (Beograd)')
get_club_seasonal_performance(serbia_data_tidy, 'OFK Beograd (Beograd)')
get_club_seasonal_performance(serbia_data_tidy, 'Čukarički (Beograd)')
get_club_seasonal_performance(serbia_data_tidy, 'Spartak ŽK (Subotica)')
get_club_seasonal_performance(serbia_data_tidy, 'Radnički (Niš)')
get_club_seasonal_performance(serbia_data_tidy, 'Rad (Beograd)')

get_club_seasonal_performance(montenegro_data_tidy, 'Sutjeska (Nikšić)')
get_club_seasonal_performance(montenegro_data_tidy, 'Budućnost (Podgorica)')
get_club_seasonal_performance(montenegro_data_tidy, 'Rudar (Pljevlja)')
get_club_seasonal_performance(montenegro_data_tidy, 'Zeta (Golubovci)')
get_club_seasonal_performance(montenegro_data_tidy, 'Grbalj (Radanovići)')
get_club_seasonal_performance(montenegro_data_tidy, 'OFK Petrovac (Petrovac)')

# Get end of the season table -------------------------------------------------

season_ending_table_srb <- get_season_ending_table(serbia_data_tidy)
season_ending_table_mne <- get_season_ending_table(montenegro_data_tidy)

# Get statistics for the end of the season ------------------------------------

season_ending_stats_srb <- get_season_ending_stat(season_ending_table_srb)
season_ending_stats_mne <- get_season_ending_stat(season_ending_table_mne)

View(season_ending_stats_srb, title='Season Ending Stats SRB')
View(season_ending_stats_mne, title='Season Ending Stats MNE')

# Plot end of season statistics -----------------------------------------------

plot_season_ending(season_ending_stats_srb)
plot_season_ending(season_ending_stats_mne)

# Plot end of season statistics using joyplot ---------------------------------

joyplot_points(season_ending_table_srb)
joyplot_points(season_ending_table_mne)

# Get data for opening weekend results ----------------------------------------

opening_weekend_srb <- get_opening_weekend(serbia_data_tidy)
opening_weekend_mne <- get_opening_weekend(montenegro_data_tidy)

View(opening_weekend_srb, title='Opening Weekend Results Serbia')
View(opening_weekend_mne, title='Opening Weekend Results Montenegro')

# See if home field advantage somehow affects opening weekend results ---------

opening_home_adv_srb <- get_opening_weekend_home_adv(opening_weekend_srb)
opening_home_adv_mne <- get_opening_weekend_home_adv(opening_weekend_mne)

View(opening_home_adv_srb, title='Home Field Advantage Opening Games Serbia')
View(opening_home_adv_mne, title='Home Field Advantage Opening Games Montenegro')

# See if home field advantage somehow affects results in general ---------------

home_adv_srb <- get_home_adv_season(serbia_data_tidy)
home_adv_mne <- get_home_adv_season(montenegro_data_tidy)

View(home_adv_srb, title='Home Field Advantage Whole Season Serbia')
View(home_adv_mne, title='Home Field Advantage Whole Season Montenegro')

# See win percentage of opening games through season ---------------------------

get_seasonal_opening_percentage(opening_weekend_srb)
get_seasonal_opening_percentage(opening_weekend_mne)

# See home field advantage per team --------------------------------------------

home_team_adv_srb <- get_home_adv_team(opening_weekend_srb)
home_team_adv_mne<- get_home_adv_team(opening_weekend_mne)

View(home_team_adv_srb, title='Home Team Advantage Per Serbian Team')
View(home_team_adv_mne, title='Home Team Advantage Per Montenegrin Team')

# Plot win percentage for home and away games on opening weekend per team ------

get_home_away_win_percent(home_team_adv_srb)
get_home_away_win_percent(opening_weekend_mne)

# Plot win percentage overall for teams on opening weekend ---------------------

get_overall_win_percent(opening_weekend_srb)
get_overall_win_percent(opening_weekend_mne)

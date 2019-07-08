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
}))

# Load functions for data manipulation ----------------------------------------

source("manipulate_data.R")
source("descriptive.R")

# Load the data file ----------------------------------------------------------

serbia_csv <- list.files(path = 'SRB', full.names = TRUE)
montengro_csv <- list.files(path = 'MNE', full.names = TRUE)

# Map the data from file ------------------------------------------------------

serbia_raw_data <- map(serbia_csv, read.csv)
montenegro_raw_data <- map(montengro_csv, read.csv)

# Convert data to tibble ------------------------------------------------------

serbia_data <- do.call(rbind, serbia_raw_data) %>% as_tibble()
montenegro_data <- do.call(rbind, montenegro_raw_data) %>% as_tibble()

## MANIPULATE DATA ------------------------------------------------------------
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

# Replace missing Date values -------------------------------------------------

serbia_data <- replace_missing_dates(serbia_data)
montenegro_data <- replace_missing_dates(montenegro_data)

# Correct incorrect Date values -----------------------------------------------

incorrect_date_level <- serbia_data$Level[year(serbia_data$Date)==2000]
incorrect_date_matchday <- serbia_data$Matchday[year(serbia_data$Date)==2000]
incorrect_date_season <- serbia_data$Season[year(serbia_data$Date)==2000]

serbia_data$Date[year(serbia_data$Date)==2000] <- serbia_data$Date[year(serbia_data$Date)!=2000
                 & serbia_data$Level == incorrect_date_level
                 & serbia_data$Matchday == incorrect_date_matchday
                 & serbia_data$Season == incorrect_date_season][1]

# Default missing resaults to 0:0 as to not affect Goal difference too much ---

montenegro_data$HomeGoals[is.na(montenegro_data$HomeGoals)] <- 0
montenegro_data$AwayGoals[is.na(montenegro_data$AwayGoals)] <- 0
serbia_data$HomeGoals[is.na(serbia_data$HomeGoals)] <- 0
serbia_data$AwayGoals[is.na(serbia_data$AwayGoals)] <- 0

# View Data -------------------------------------------------------------------

view(serbia_data)
view(montenegro_data)

# Get seasonal data -----------------------------------------------------------

serbia_data_tidy <- seasonal_result_data(serbia_data[serbia_data$Level==1,])
montenegro_data_tidy <- seasonal_result_data(montenegro_data)

# CONDUCT STATISTICAL ANALYSIS ------------------------------------------------
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

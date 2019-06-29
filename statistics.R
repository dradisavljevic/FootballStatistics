# Set working directory to the current files directory ------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading in the required packages --------------------------------------------
install.packages('tidyr')
install.packages('lubridate')
install.packages('magrittr')
install.packages('tidyquant')
install.packages('purrr')
install.packages('dplyr')
suppressWarnings(suppressPackageStartupMessages({
  library(tidyr)
  library(lubridate)
  library(magrittr)
  library(tidyquant)
  library(purrr)
  library(dplyr)
  library(stringr)
}))

# Load the data file ----------------------------------------------------------
serbia_csv <- list.files(path = 'SRB', full.names = TRUE)
montengro_csv <- list.files(path = 'MNE', full.names = TRUE)

# Map the data from file ------------------------------------------------------
serbia_raw_data <- map(serbia_csv, read.csv)
montenegro_raw_data <- map(montengro_csv, read.csv)

# Convert data to tibble ------------------------------------------------------
serbia_data <- do.call(rbind, serbia_raw_data) %>% as_tibble()
montenegro_data <- do.call(rbind, montenegro_raw_data) %>% as_tibble()

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
str_to_date <- function(date) {
  date <- gsub('\\.', '/', date)
  date <- dmy(date)
  
  return(date)
}

serbia_data$Date <- str_to_date(serbia_data$Date)
montenegro_data$Date <- str_to_date(montenegro_data$Date)

# Function that spells the cities grammarly correctly -------------------------
capitalize_words <- function(city_name) {
  city_name <- as.character(city_name)
  capitalized_name <- strsplit(city_name, ' ')[[1]]
  capitalized_name <- paste(toupper(substring(capitalized_name, 1,1)),
                            substring(capitalized_name, 2), 
                            sep='', collapse=' ')
  return(capitalized_name)
}

serbia_data$HostCity <- sapply(serbia_data$HostCity,
                               capitalize_words)

serbia_data$GuestCity <- sapply(serbia_data$GuestCity,
                                capitalize_words)

montenegro_data$HostCity <- sapply(montenegro_data$HostCity,
                                   capitalize_words)

montenegro_data$GuestCity <- sapply(montenegro_data$GuestCity,
                                    capitalize_words)

# Merge city columns with club name ones ---------------------------------------

serbia_data$Host <- str_split_fixed(serbia_data$Host, '\\(', 2)[,1]
montenegro_data$Host <- str_split_fixed(montenegro_data$Host, '\\(', 2)[,1]
serbia_data$Guest <- str_split_fixed(serbia_data$Guest, '\\(', 2)[,1]
montenegro_data$Guest <- str_split_fixed(montenegro_data$Guest, '\\(', 2)[,1]

team_id_vector <- c()
team_name_vector <- c()

by(serbia_data, 1:nrow(serbia_data), function(row) {
  if (!(row$HostID) %in% team_id_vector){
    team_id_vector[length(team_id_vector)+1] <<- row$HostID
    team_name_vector[length(team_name_vector)+1] <<-
      paste(row$Host, ' (',row$HostCity, ')', sep='')
  }
})

for (i in seq(1,length(team_id_vector))){
  serbia_data$Host <- ifelse(serbia_data$HostID==team_id_vector[i],
                        as.character(team_name_vector[i]),
                        as.character(serbia_data$Host))
  serbia_data$Guest <- ifelse(serbia_data$GuestID==team_id_vector[i],
                      as.character(team_name_vector[i]),
                      as.character(serbia_data$Guest))
}

team_id_vector <- c()
team_name_vector <- c()

by(montenegro_data, 1:nrow(montenegro_data), function(row) {
  if (!(row$HostID) %in% team_id_vector){
    team_id_vector[length(team_id_vector)+1] <<- row$HostID
    team_name_vector[length(team_name_vector)+1] <<-
      paste(row$Host, ' (',row$HostCity, ')', sep='')
  }
})

for (i in seq(1,length(team_id_vector))){
  montenegro_data$Host <- ifelse(montenegro_data$HostID==team_id_vector[i],
                             as.character(team_name_vector[i]),
                             as.character(montenegro_data$Host))
  montenegro_data$Guest <- ifelse(montenegro_data$GuestID==team_id_vector[i],
                              as.character(team_name_vector[i]),
                              as.character(montenegro_data$Guest))
}

# Remove whitespace from club name and city columns ---------------------------
remove_white_space <- function(df) {
  df$Host <- trimws(gsub('\\s+', ' ', df$Host),
                    'both', whitespace = '[ \t\r\n]')
  
  df$Guest <- trimws(gsub('\\s+', ' ', df$Guest),
                     'both', whitespace = '[ \t\r\n]')
  
  df$League <- trimws(gsub('\\s+', ' ', df$League),
                      'both', whitespace = '[ \t\r\n]')
  return (df)
}

serbia_data <- remove_white_space(serbia_data)
montenegro_data <- remove_white_space(montenegro_data)

# Take the end year of a season as season year --------------------------------
round_up_season <- function(df) {
  df$Season <- as.numeric(substr(df$Season, 6, 9))
  return (df)
}

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
                      HostID,
                      Guest,
                      GuestID,
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
                          HostID,
                          Guest,
                          GuestID,
                          HomeGoals,
                          AwayGoals,
                          Outcome)

# View Data -------------------------------------------------------------------
view(serbia_data)
view(montenegro_data)

# Check for not defined values ------------------------------------------------
serbia_data %>% 
  summarise_all(function(x) sum(is.na(x)))

montenegro_data %>% 
  summarise_all(function(x) sum(is.na(x)))

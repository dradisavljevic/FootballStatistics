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
serbia_csv <- list.files(path = "SRB", full.names = TRUE)
montengro_csv <- list.files(path = "MNE", full.names = TRUE)

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
  date <- gsub("\\.", "/", date)
  date <- dmy(date)
  
  return(date)
}

serbia_data$Date <- str_to_date(serbia_data$Date)
montenegro_data$Date <- str_to_date(montenegro_data$Date)

# Remove whitespace from club name and city columns ---------------------------
remove_white_space <- function(df) {
  df$Host <- trimws(gsub('\\s+', ' ', df$Host),
                    'both', whitespace = "[ \t\r\n]")
  
  df$HostCity <- trimws(gsub('\\s+', ' ', df$HostCity),
                        'both', whitespace = "[ \t\r\n]")
  
  df$Guest <- trimws(gsub('\\s+', ' ', df$Guest),
                     'both', whitespace = "[ \t\r\n]")
  
  df$GuestCity <- trimws(gsub('\\s+', ' ', df$GuestCity),
                         'both', whitespace = "[ \t\r\n]")
  
  df$League <- trimws(gsub('\\s+', ' ', df$League),
                      'both', whitespace = "[ \t\r\n]")
  return (df)
}

serbia_data <- remove_white_space(serbia_data)
montenegro_data <- remove_white_space(montenegro_data)

# Function that spells the cities grammarly correctly -------------------------
capitalize_words <- function(city_name) {
  city_name <- as.character(city_name)
  capitalized_name <- strsplit(city_name, " ")[[1]]
  capitalized_name <- paste(toupper(substring(capitalized_name, 1,1)),
                            substring(capitalized_name, 2), 
                            sep="", collapse=" ")
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

# Take the end year of a season as season year --------------------------------
round_up_season <- function(df) {
  df$Season <- as.numeric(substr(df$Season, 6, 9))
  return (df)
}

serbia_data <- round_up_season(serbia_data)
montenegro_data <- round_up_season(montenegro_data)

# Remove URL column -----------------------------------------------------------
serbia_data <- select(serbia_data,
                      League,
                      Level,
                      Season,
                      Matchday,
                      Date,
                      Time,
                      Host,
                      HostCity,
                      HostID,
                      Guest,
                      GuestCity,
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
                          HostCity,
                          HostID,
                          Guest,
                          GuestCity,
                          GuestID,
                          HomeGoals,
                          AwayGoals,
                          Outcome)

# View Data -------------------------------------------------------------------
view(serbia_data)
view(montenegro_data)

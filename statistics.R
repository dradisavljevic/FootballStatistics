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
simple_names <- c('league',
                  'level',
                  'season',
                  'matchday',
                  'date',
                  'time',
                  'host_id',
                  'host',
                  'host_city',
                  'host_url',
                  'guest_id',
                  'guest',
                  'guest_city',
                  'guest_url',
                  'home_goals',
                  'away_goals')

# Replace names row -----------------------------------------------------------
names(serbia_data) <- simple_names
names(montenegro_data) <- simple_names

# Convert date from String to Date type ---------------------------------------
str_to_date <- function(date) {
  date <- gsub("\\.", "/", date)
  date <- dmy(date)
  
  return(date)
}

serbia_data$date <- str_to_date(serbia_data$date)
montenegro_data$date <- str_to_date(montenegro_data$date)

# Remove whitespace from club name and city columns ---------------------------
remove_white_space <- function(df) {
  df$host <- trimws(gsub('\\s+', ' ', df$host),
                    'both', whitespace = "[ \t\r\n]")
  
  df$host_city <- trimws(gsub('\\s+', ' ', df$host_city),
                         'both', whitespace = "[ \t\r\n]")
  
  df$guest <- trimws(gsub('\\s+', ' ', df$guest),
                     'both', whitespace = "[ \t\r\n]")
  
  df$guest_city <- trimws(gsub('\\s+', ' ', df$guest_city),
                          'both', whitespace = "[ \t\r\n]")
  
  df$league <- trimws(gsub('\\s+', ' ', df$league),
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

serbia_data$host_city <- sapply(serbia_data$host_city,
                                capitalize_words)

serbia_data$guest_city <- sapply(serbia_data$guest_city,
                                 capitalize_words)

montenegro_data$host_city <- sapply(montenegro_data$host_city,
                                    capitalize_words)

montenegro_data$guest_city <- sapply(montenegro_data$guest_city,
                                     capitalize_words)

# Take the end year of a season as season year --------------------------------
round_up_season <- function(df) {
  df$season <- as.numeric(substr(df$season, 6, 9))
  return (df)
}

serbia_data <- round_up_season(serbia_data)
montenegro_data <- round_up_season(montenegro_data)

# Remove URL column -----------------------------------------------------------
serbia_data <- select(serbia_data,
                      league,
                      level,
                      season,
                      matchday,
                      date,
                      time,
                      host,
                      host_city,
                      host_id,
                      guest,
                      guest_city,
                      guest_id,
                      home_goals,
                      away_goals)

montenegro_data <- select(montenegro_data, 
                          league, 
                          level, 
                          season, 
                          matchday, 
                          date, 
                          time, 
                          host, 
                          host_city, 
                          host_id, 
                          guest, 
                          guest_city, 
                          guest_id, 
                          home_goals, 
                          away_goals)

# View Data -------------------------------------------------------------------
view(serbia_data)
view(montenegro_data)


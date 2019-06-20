# Set working directory to the current files directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading in the required packages
install.packages('tidyr')
install.packages('ggplot2')
install.packages('lubridate')
install.packages('magrittr')
install.packages('tidyquant')
install.packages('purrr')
install.packages('ggjoy')
install.packages('dplyr')
suppressWarnings(suppressPackageStartupMessages({
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(magrittr)
  library(tidyquant)
  library(purrr)
  library(ggjoy)
  library(dplyr)
}))

# Load the data file
serbia_csv <- list.files(path = "SRB", full.names = TRUE)
montengro_csv <- list.files(path = "MNE", full.names = TRUE)

# Map the data from file
serbia_raw_data <- map(serbia_csv, read.csv)
montenegro_raw_data <- map(montengro_csv, read.csv)

# Convert data to tibble
serbia_data <- do.call(rbind, serbia_raw_data) %>% as_tibble()
montenegro_data <- do.call(rbind, montenegro_raw_data) %>% as_tibble()

# Variable for new dataframe names
simple_names <- c('league', 'level', 'season', 'matchday', 'date', 'time', 'hostID', 'host', 'hostCity', 'hostURL', 'guestID', 'guest', 'guestCity', 'guestURL', 'homeGoals', 'awayGoals')

# Replace names row
names(serbia_data) <- simple_names
names(montenegro_data) <- simple_names

# Convert date from String to Date type
strToDate <- function(df){
  df <- gsub("\\.", "/", df)
  df <- dmy(df)
  
  return(df)
}

serbia_data$date <- strToDate(serbia_data$date)
montenegro_data$date <- strToDate(montenegro_data$date)

# View Data
view(serbia_data)

# Remove whitespace from Club name and city columns
removeWS <- function(df) {
  df$host <- trimws(df$host, 'both', whitespace = "[ \t\r\n]")
  df$hostCity <- trimws(df$hostCity, 'both', whitespace = "[ \t\r\n]")
  df$guest <- trimws(df$guest, 'both', whitespace = "[ \t\r\n]")
  df$guestCity <- trimws(df$guestCity, 'both', whitespace = "[ \t\r\n]")
  return (df)
}

serbia_data <- removeWS(serbia_data)
montenegro_data <- removeWS(montenegro_data)

# URL column is not necessary
serbia_data <- select(serbia_data, league, level, season, matchday, date, time, host, hostCity, hostID, guest, guestCity, guestID, homeGoals, awayGoals)
montenegro_data <- select(montenegro_data, league, level, season, matchday, date, time, host, hostCity, hostID, guest, guestCity, guestID, homeGoals, awayGoals)


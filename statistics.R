## SET UP ENVIRONMENT AND LOAD DATA
# Set working directory to the current files directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install necessary packages
install.packages("tidyr")
install.packages("lubridate")
install.packages("magrittr")
install.packages("tidyquant")
install.packages("purrr")
install.packages("dplyr")
install.packages("moments")
install.packages("ggplot2")
install.packages("ggjoy")
install.packages("nortest")

# Loading in the required packages
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

# Load functions from other source files
source("prepare_data.R")
source("descriptive.R")
source("match_result_stat.R")
source("opening_weekend.R")

# Load the csv data
serbiaCSV <- list.files(path = "SRB", full.names = TRUE)
montenegroCSV <- list.files(path = "MNE", full.names = TRUE)

# Map the data to dataframe
serbiaRawData <- map(serbiaCSV, read.csv)
montenegroRawData <- map(montenegroCSV, read.csv)

# Convert mapped data to tibble
serbiaData <- do.call(rbind, serbiaRawData) %>% as_tibble()
montenegroData <- do.call(rbind, montenegroRawData) %>% as_tibble()

## PREPARE DATA
# Specify new column names
simpleNames <- c("League",
                  "Level",
                  "Season",
                  "Matchday",
                  "Date",
                  "Time",
                  "HostID",
                  "Host",
                  "HostCity",
                  "HostURL",
                  "GuestID",
                  "Guest",
                  "GuestCity",
                  "GuestURL",
                  "HomeGoals",
                  "AwayGoals",
                  "HomeGoalsHalf",
                  "AwayGoalsHalf",
                  "Outcome")

# Replace column names in dataframes
names(serbiaData) <- simpleNames
names(montenegroData) <- simpleNames

# Convert date from string value to date format
serbiaData$Date <- GetDateFromString(serbiaData$Date)
montenegroData$Date <- GetDateFromString(montenegroData$Date)

# Correct the capitalization on club name and city columns
serbiaData$HostCity <- sapply(serbiaData$HostCity,
                              CapitalizeWords)

serbiaData$Host <- sapply(serbiaData$Host,
                          CapitalizeWords)

montenegroData$Host <- sapply(montenegroData$Host,
                              CapitalizeWords)

montenegroData$HostCity <- sapply(montenegroData$HostCity,
                                  CapitalizeWords)

# Merge city columns with club name ones
serbiaData$Host <- str_split_fixed(serbiaData$Host, "\\(", 2)[,1]
montenegroData$Host <- str_split_fixed(montenegroData$Host, "\\(", 2)[,1]
serbiaData$Guest <- str_split_fixed(serbiaData$Guest, "\\(", 2)[,1]
montenegroData$Guest <- str_split_fixed(montenegroData$Guest, "\\(", 2)[,1]

serbiaData <- MergeClubAndCityNames(serbiaData)
montenegroData <- MergeClubAndCityNames(montenegroData)

# Remove trailing and leading whitespaces from club name and league columns
serbiaData <- RemoveWhiteSpace(serbiaData)
montenegroData <- RemoveWhiteSpace(montenegroData)

# Round up the season column to a single year
serbiaData <- RoundUpSeason(serbiaData)
montenegroData <- RoundUpSeason(montenegroData)

# Remove columns that won't be used in analysis
serbiaData <- select(serbiaData,
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

montenegroData <- select(montenegroData, 
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

# Check for presence of NA values
serbiaData %>% 
  summarise_all(function(x) sum(is.na(x)))

montenegroData %>% 
  summarise_all(function(x) sum(is.na(x)))

# There is a single date value that has been scraped that does not fit in the
# correct season. Correct this date by approximating the date value of the match
# to those of the same matchday.
incorrectDateLevel <- serbiaData$Level[!is.na(serbiaData$Date) & 
                                            year(serbiaData$Date)==2000]
incorrectDateMatchday <- serbiaData$Matchday[!is.na(serbiaData$Date) & 
                                                  year(serbiaData$Date)==2000]
incorrectDateSeason <- serbiaData$Season[!is.na(serbiaData$Date) & 
                                              year(serbiaData$Date)==2000]

serbiaData$Date[year(serbiaData$Date)==2000] <- serbiaData$Date[year(serbiaData$Date)!=2000
                 & serbiaData$Level == incorrectDateLevel
                 & serbiaData$Matchday == incorrectDateMatchday
                 & serbiaData$Season == incorrectDateSeason][1]

# Display prepared data
View(serbiaData, title="Serbia Football Data")
View(montenegroData,  title="Montenegro Football Data")

## CONDUCT STATISTICAL ANALYSIS
# Get descriptive statistics by using functions from the descriptive.R source
GetTeamsPerLevel(serbiaData)
GetTeamsPerLevel(montenegroData)
GetClubCountPerCity(serbiaData)
GetClubCountPerCity(montenegroData)

GetDescriptiveStatsHomeGoals(serbiaData)
GetDescriptiveStatsHomeGoals(montenegroData)
GetDescriptiveStatsAwayGoals(serbiaData)
GetDescriptiveStatsAwayGoals(montenegroData)
GetDescriptiveStatsCombinedGoals(serbiaData)
GetDescriptiveStatsCombinedGoals(montenegroData)

GetMaximumGoalsMatchday(serbiaData)
GetMaximumGoalsMatchday(montenegroData)
GetMinimumGoalsPerMatchday(serbiaData)
GetMinimumGoalsPerMatchday(montenegroData)

GetWinPercentagePerLeague(serbiaData)
GetWinPercentagePerLeague(montenegroData)
GetWinPercentagePerLeagueMatchday(serbiaData[serbiaData$Level==1,])
GetWinPercentagePerLeagueMatchday(montenegroData[montenegroData$Level==1,])

GetMinMaxGoalsPerTeam(serbiaData)
GetMinMaxGoalsPerTeam(montenegroData)

GetWinPercentagePerDay(serbiaData)
GetWinPercentagePerDay(montenegroData)

GetDescritiveStatsDayHome(serbiaData)
GetDescritiveStatsDayHome(montenegroData)
GetDescritiveStatsDayAway(serbiaData)
GetDescritiveStatsDayAway(montenegroData)
GetDescritiveStatsDayCombined(serbiaData)
GetDescritiveStatsDayCombined(montenegroData)

# Create new variable that will be used for seasonal data analysis
serbiaDataSeasonal <- serbiaData
montenegroDataSeasonal <- montenegroData

# Default missing resaults to 0:0 as to not affect Goal difference too much 
montenegroDataSeasonal$HomeGoals[is.na(montenegroDataSeasonal$HomeGoals)] <- 0
montenegroDataSeasonal$AwayGoals[is.na(montenegroDataSeasonal$AwayGoals)] <- 0
serbiaDataSeasonal$HomeGoals[is.na(serbiaDataSeasonal$HomeGoals)] <- 0
serbiaDataSeasonal$AwayGoals[is.na(serbiaDataSeasonal$AwayGoals)] <- 0

# Replace missing Date values
serbiaDataSeasonal <- ReplaceMissingDates(serbiaDataSeasonal)
montenegroDataSeasonal <- ReplaceMissingDates(montenegroDataSeasonal)

# Get seasonal data in tidy format
serbiaDataTidy <- GetSeasonalResultData(serbiaDataSeasonal[serbiaDataSeasonal$Level==1,])
montenegroDataTidy <- GetSeasonalResultData(montenegroDataSeasonal[montenegroDataSeasonal$Level==1,])

# Get seasonal performance by club
GetClubSeasonalPerformance(serbiaDataTidy, "Partizan (Beograd)")
GetClubSeasonalPerformance(serbiaDataTidy, "Crvena Zvezda (Beograd)")
GetClubSeasonalPerformance(serbiaDataTidy, "Vojvodina (Novi Sad)")
GetClubSeasonalPerformance(serbiaDataTidy, "OFK Beograd (Beograd)")
GetClubSeasonalPerformance(serbiaDataTidy, "Čukarički (Beograd)")
GetClubSeasonalPerformance(serbiaDataTidy, "Spartak ŽK (Subotica)")
GetClubSeasonalPerformance(serbiaDataTidy, "Radnički (Niš)")
GetClubSeasonalPerformance(serbiaDataTidy, "Rad (Beograd)")
GetClubSeasonalPerformance(serbiaDataTidy, "Jagodina (Jagodina)")

GetClubSeasonalPerformance(montenegroDataTidy, "Sutjeska (Nikšić)")
GetClubSeasonalPerformance(montenegroDataTidy, "Budućnost (Podgorica)")
GetClubSeasonalPerformance(montenegroDataTidy, "Rudar (Pljevlja)")
GetClubSeasonalPerformance(montenegroDataTidy, "Zeta (Golubovci)")
GetClubSeasonalPerformance(montenegroDataTidy, "Grbalj (Radanovići)")
GetClubSeasonalPerformance(montenegroDataTidy, "OFK Petrovac (Petrovac)")

# Get end of the season table
seasonEndingTableSRB <- GetSeasonEndingTable(serbiaDataTidy)
seasonEndingTableMNE <- GetSeasonEndingTable(montenegroDataTidy)

# Get descriptive statistics for the end of the season
seasonEndingStatsSRB <- GetSeasonEndingStatistics(seasonEndingTableSRB)
seasonEndingStatsMNE <- GetSeasonEndingStatistics(seasonEndingTableMNE)

View(seasonEndingStatsSRB, title="Season Ending Stats SRB")
View(seasonEndingStatsMNE, title="Season Ending Stats MNE")

# Plot end of season statistics
PlotEndOfSeasonData(seasonEndingStatsSRB)
PlotEndOfSeasonData(seasonEndingStatsMNE)

# Plot end of season statistics using joyplot library
JoyplotEndOfSeasonData(seasonEndingTableSRB)
JoyplotEndOfSeasonData(seasonEndingTableMNE)

# Get opening weekend results inside the dataframe
openingWeekendSRB <- GetOpeningWeekendData(serbiaDataTidy)
openingWeekendMNE <- GetOpeningWeekendData(montenegroDataTidy)

View(openingWeekendSRB, title="Opening Weekend Results Serbia")
View(openingWeekendMNE, title="Opening Weekend Results Montenegro")

# Calculate win percentage for games played on home venue
openingHomeAdvSRB <- GetOpeningWeekendHomeAdvantage(openingWeekendSRB)
openingHomeAdvMNE <- GetOpeningWeekendHomeAdvantage(openingWeekendMNE)

View(openingHomeAdvSRB, title="Home Field Advantage Opening Games Serbia")
View(openingHomeAdvMNE, title="Home Field Advantage Opening Games Montenegro")

# See if home field advantage somehow affects results in general
homeSeasonAdvSRB <- GetSeasonalHomeAdvantage(serbiaDataTidy)
homeSeasonAdvMNE <- GetSeasonalHomeAdvantage(montenegroDataTidy)

View(homeSeasonAdvSRB, title="Home Field Advantage Whole Season Serbia")
View(homeSeasonAdvMNE, title="Home Field Advantage Whole Season Montenegro")

# Display win percentage for opening games through seasons
PlotWinPercentageSeasonal(openingWeekendSRB)
PlotWinPercentageSeasonal(openingWeekendMNE)

# Calculate home venue win percentage per team
homeTeamAdvSRB <- GetHomeAdvantagePerTeam(openingWeekendSRB)
homeTeamAdvMNE<- GetHomeAdvantagePerTeam(openingWeekendMNE)

View(homeTeamAdvSRB, title="Home Team Advantage Per Serbian Team")
View(homeTeamAdvMNE, title="Home Team Advantage Per Montenegrin Team")

# Plot win percentage for home and away games on opening weekend per team
GetWinPercentageHomeAndAway(homeTeamAdvSRB)
GetWinPercentageHomeAndAway(homeTeamAdvMNE)

# Plot win percentage overall for teams on opening weekend
GetOverallWinPercentage(openingWeekendSRB)
GetOverallWinPercentage(openingWeekendMNE)

ad.test(serbiaData$HomeGoals[serbiaData$Level==4])

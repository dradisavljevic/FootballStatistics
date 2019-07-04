# Convert date from String to Date type ----------------------------------------
str_to_date <- function(date) {
  date <- gsub('\\.', '/', date)
  date <- dmy(date)
  
  return(date)
}

# Function that spells the cities and club names grammarly correctly -----------
capitalize_words <- function(city_name) {
  city_name <- as.character(city_name)
  capitalized_name <- strsplit(city_name, ' ')[[1]]
  capitalized_name <- paste(toupper(substring(capitalized_name, 1,1)),
                            substring(capitalized_name, 2), 
                            sep='', collapse=' ')
  return(capitalized_name)
}

# Remove whitespace from club name and city columns ----------------------------
remove_white_space <- function(df) {
  df$Host <- trimws(gsub('\\s+', ' ', df$Host),
                    'both', whitespace = '[ \t\r\n]')
  
  df$Guest <- trimws(gsub('\\s+', ' ', df$Guest),
                     'both', whitespace = '[ \t\r\n]')
  
  df$League <- trimws(gsub('\\s+', ' ', df$League),
                      'both', whitespace = '[ \t\r\n]')
  return (df)
}

# Take the end year of a season as season year ---------------------------------
round_up_season <- function(df) {
  df$Season <- as.numeric(substr(df$Season, 6, 9))
  return (df)
}

# Get seasonal data from desired league ----------------------------------------
seasonal_result_data <- function(data) {
  seasonal_data <- data %>%
    gather(key = "Venue", value = Team, Host:Guest) %>% 
    arrange(Date) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(Venue = ifelse(Venue == "Host",
                          "Home",
                          "Away"),
           Outcome = case_when(Venue == "Home" & Outcome == "H" ~ "W",
                               Venue == "Home" & Outcome == "G" ~ "L",
                               Venue == "Away" & Outcome == "H" ~ "L",
                               Venue == "Away" & Outcome == "G" ~ "W",
                               TRUE ~ Outcome),
           FTGF = ifelse(Venue == "Home", HomeGoals, AwayGoals),  #Full Time Goals For
           FTGA = ifelse(Venue == "Home", AwayGoals, HomeGoals),  #Full Time Goals Against
           GoalDifference = FTGF - FTGA,                    #goal difference
           PointsEarned = case_when(Outcome == "W" ~ 3,           #adding points
                                     Outcome == "D" ~ 1,
                                     Outcome == "L" ~ 0)) %>% 
    select(League, Season, Date, Team, Venue, Outcome, FTGF, 
           FTGA, GoalDifference, PointsEarned) %>%
    group_by(Season, League, Team) %>%
    mutate(points = cumsum(PointsEarned),
           GoalDifferenceTotal = cumsum(GoalDifference)) %>% #calculating the number of points each team has through out the season
    ungroup()
  
  return (seasonal_data)
}
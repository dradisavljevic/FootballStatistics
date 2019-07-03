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
    gather(key = "venue", value = team, Host:Guest) %>% 
    arrange(Date) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(venue = ifelse(venue == "Host",
                          "Home",
                          "Away"),
           Outcome = case_when(venue == "Home" & Outcome == "H" ~ "W",
                               venue == "Home" & Outcome == "G" ~ "L",
                               venue == "Away" & Outcome == "H" ~ "L",
                               venue == "Away" & Outcome == "G" ~ "W",
                               TRUE ~ Outcome),
           FTGF = ifelse(venue == "Home", HomeGoals, AwayGoals),  #Full Time Goals For
           FTGA = ifelse(venue == "Home", AwayGoals, HomeGoals),  #Full Time Goals Against
           goal_diff = FTGF - FTGA,                    #goal difference
           points_earned = case_when(Outcome == "W" ~ 3,           #adding points
                                     Outcome == "D" ~ 1,
                                     Outcome == "L" ~ 0)) %>% 
    select(League, Season, Date, team, venue, Outcome, FTGF, 
           FTGA, goal_diff, points_earned) %>%
    group_by(Season, League, team) %>%
    mutate(points = cumsum(points_earned),
           goal_diff_tot = cumsum(goal_diff)) %>% #calculating the number of points each team has through out the season
    ungroup()
  
  return (seasonal_data)
}
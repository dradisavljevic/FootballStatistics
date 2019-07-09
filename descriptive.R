# Function to calculate mode ---------------------------------------------------

getMode <- function(v) {
  uv <- na.omit(v)
  uv <- unique(v)
  mode <- uv[which.max(tabulate(match(v, uv)))]
  
  return(mode)
}

# Get number of different teams that have played in the league -----------------

get_teams_per_level <- function(df) {
  number_of_levels <- max(df$Level, na.rm = TRUE)
  for (i in seq(1,number_of_levels)){
    team_number <- nrow(table(df$Host[df$Level==i]))
    print(paste('Leagues at level ', i, ' had ', team_number, ' different teams playing in it so far.'))
  }
}

# Get minimum and maximum of goals per team by season ---------------------------

get_min_max_goals_team <- function(df) {
  goals <- df %>%
    group_by(Season, Guest) %>%
    summarise(Goals = sum(AwayGoals, na.rm=TRUE),
              NumberOfGames = n(),
              Mean = mean(AwayGoals, na.rm=TRUE)) %>%
    ungroup()
  
  goals <- goals[with(goals,order(Goals)),]
  print('Teams that scored least away goals per season are: ')
  print(goals[1:10,])
  print('--------------------------------------------------------')
  print('Teams that scored most away goals per season are: ')
  goals <- goals[with(goals,order(-Goals)),]
  print(goals[1:10,])
  print('--------------------------------------------------------')
  
  goals <- df %>%
    group_by(Season, Host) %>%
    summarise(Goals = sum(HomeGoals, na.rm=TRUE),
              NumberOfGames = n(),
              Mean = mean(HomeGoals, na.rm=TRUE)) %>%
    ungroup()
  
  goals <- goals[with(goals,order(Goals)),]
  print('Teams that scored least home goals per season are: ')
  print(goals[1:10,])
  print('--------------------------------------------------------')
  print('Teams that scored most home goals per season are: ')
  goals <- goals[with(goals,order(-Goals)),]
  print(goals[1:10,])
  print('--------------------------------------------------------')
}

# Count number of clubs per city -----------------------------------------------

get_number_of_clubs_per_city <- function(df) {
  df <- df %>%
    group_by(Host) %>%
    summarise(NumberOfGames = n()) %>%
    ungroup()
  
  suppressWarnings(
    df <- df %>% separate(Host, sep = "\\(", into = c("ClubName", "CityName") )
  )
  
  df$CityName <- gsub('\\)', '', df$CityName)
  
  df <- df %>%
    group_by(CityName) %>%
    summarise(NumberOfClubs = n()) %>%
    ungroup()
  
  df <- df[df$NumberOfClubs > 1,]
  
  df <- df[with(df,order(-NumberOfClubs)),]
  
  View(df, title='Cities with more than one club participating in leagues: ')
  print(df)
  
}

# Get descriptive statistics for goals scored per league season ----------------

get_home_goals_descriptive <- function(df) {
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(Max = max(HomeGoals, na.rm=TRUE),
              Min = min(HomeGoals, na.rm=TRUE),
              Mean = mean(HomeGoals, na.rm=TRUE),
              StandardDeviation = sd(HomeGoals, na.rm=TRUE),
              SDMeanError = sd(HomeGoals, na.rm=TRUE) / n(),
              Median = quantile(HomeGoals, na.rm=TRUE)[3],
              Mode = getMode(HomeGoals),
              Quantile1 = quantile(HomeGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(HomeGoals, na.rm=TRUE)[4],
              Skewness = skewness(HomeGoals, na.rm=TRUE),
              Kurtosis = kurtosis(HomeGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(HomeGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  
  View(goals,title='Home Goals Per League Season Statistics')
}

get_away_goals_descriptive <- function(df) {
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(Max = max(AwayGoals, na.rm=TRUE),
              Min = min(AwayGoals, na.rm=TRUE),
              Mean = mean(AwayGoals, na.rm=TRUE),
              StandardDeviation = sd(AwayGoals, na.rm=TRUE),
              SDMeanError = sd(AwayGoals, na.rm=TRUE) / n(),
              Median = quantile(AwayGoals, na.rm=TRUE)[3],
              Mode = getMode(AwayGoals),
              Quantile1 = quantile(AwayGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(AwayGoals, na.rm=TRUE)[4],
              Skewness = skewness(AwayGoals, na.rm=TRUE),
              Kurtosis = kurtosis(AwayGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(AwayGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  
  View(goals,title='Away Goals Per League Season Statistics')
}


get_combined_goals_descriptive <- function(df) {
  df <- df %>%
    mutate(CombinedGoals = HomeGoals + AwayGoals)
  
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(Max = max(CombinedGoals, na.rm=TRUE),
              Min = min(CombinedGoals, na.rm=TRUE),
              Mean = mean(CombinedGoals, na.rm=TRUE),
              StandardDeviation = sd(CombinedGoals, na.rm=TRUE),
              SDMeanError = sd(CombinedGoals, na.rm=TRUE) / n(),
              Median = quantile(CombinedGoals, na.rm=TRUE)[3],
              Mode = getMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(CombinedGoals, na.rm=TRUE)[4],
              Skewness = skewness(CombinedGoals, na.rm=TRUE),
              Kurtosis = kurtosis(CombinedGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(CombinedGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  
  
  View(goals,title='Combined Goals Per League Season Statistics')
}

# Get statistics for highest and fewest scoring matchdays ----------------------

get_goal_max_per_matchday <- function(df) {
  df <- df %>%
    mutate(CombinedGoals = HomeGoals + AwayGoals)
  
  goals <- df %>%
    group_by(Season, League, Matchday) %>%
    summarise(Max = max(CombinedGoals, na.rm=TRUE),
              Min = min(CombinedGoals, na.rm=TRUE),
              Mean = mean(CombinedGoals, na.rm=TRUE),
              StandardDeviation = sd(CombinedGoals, na.rm=TRUE),
              SDMeanError = sd(CombinedGoals, na.rm=TRUE) / n(),
              Median = quantile(CombinedGoals, na.rm=TRUE)[3],
              Mode = getMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(CombinedGoals, na.rm=TRUE)[4],
              Skewness = skewness(CombinedGoals, na.rm=TRUE),
              Kurtosis = kurtosis(CombinedGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(CombinedGoals, na.rm=TRUE),
              ) %>%
    filter(Total == max(Total)) %>%
    arrange(desc(Total), .by_group = FALSE) %>%
    ungroup()
  
  View(goals,title='Highest Scoring Matchday Statistics')
}

get_goal_min_per_matchday <- function(df) {
  df <- df %>%
    mutate(CombinedGoals = HomeGoals + AwayGoals)
  
  goals <- df %>%
    group_by(Season, League, Matchday) %>%
    summarise(Max = max(CombinedGoals, na.rm=TRUE),
              Min = min(CombinedGoals, na.rm=TRUE),
              Mean = mean(CombinedGoals, na.rm=TRUE),
              StandardDeviation = sd(CombinedGoals, na.rm=TRUE),
              SDMeanError = sd(CombinedGoals, na.rm=TRUE) / n(),
              Median = quantile(CombinedGoals, na.rm=TRUE)[3],
              Mode = getMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(CombinedGoals, na.rm=TRUE)[4],
              Skewness = skewness(CombinedGoals, na.rm=TRUE),
              Kurtosis = kurtosis(CombinedGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(CombinedGoals, na.rm=TRUE),
    ) %>%
    filter(Total == min(Total)) %>%
    arrange(desc(Total), .by_group = FALSE) %>%
    ungroup()
  
  View(goals,title='Lowest Scoring Matchday Statistics')
}

# Percentage of game outcome per league season --------------------------------

get_win_percentage_league <- function(df) {
  df <- df %>%
    mutate(Result = paste(HomeGoals, ':', AwayGoals)) %>%
    group_by(Season, League) %>%
    summarise(PercentageWinsHome = round(sum(Outcome == 'H, na.rm=TRUE')/n() * 100, 2),
              PercentageDraws = round(sum(Outcome == 'D', na.rm=TRUE)/n() * 100, 2),
              PercentageWinsGuest = round(sum(Outcome == 'G', na.rm=TRUE)/n() * 100, 2),
              NumberOfGames = n(),
              MostCommonResult = getMode(Result)) %>%
    ungroup()
  
  View(df, title='Win Percentage Per League Season')
}

# Percentage of game outcome per league matchday -------------------------------

get_win_percentage_league_matchday <- function(df) {
  df <- df %>%
    mutate(Result = paste(HomeGoals, ':', AwayGoals)) %>%
    group_by(Season, League, Matchday) %>%
    summarise(PercentageWinsHome = round(sum(Outcome == 'H', na.rm=TRUE)/n() * 100, 2),
              PercentageDraws = round(sum(Outcome == 'D', na.rm=TRUE)/n() * 100, 2),
              PercentageWinsGuest = round(sum(Outcome == 'G', na.rm=TRUE)/n() * 100, 2),
              NumberOfGames = n(),
              MostCommonResult = getMode(Result)) %>%
    ungroup()
  
  View(df, title='Win Percentage Per League Matchday')
  
}

# Get Win Percentage for games of the week ------------------------------------

get_win_percentage_weekdays <- function(df) {
  df <- df %>%
    mutate(Weekday = weekdays(Date),
           Result = paste(HomeGoals, ':', AwayGoals)) %>%
    filter(!is.na(Weekday)) %>%
    group_by(Weekday) %>%
    summarise(PercentageWinsHome = round(sum(Outcome == 'H', na.rm=TRUE)/n() * 100, 2),
              PercentageDraws = round(sum(Outcome == 'D', na.rm=TRUE)/n() * 100, 2),
              PercentageWinsGuest = round(sum(Outcome == 'G', na.rm=TRUE)/n() * 100, 2),
              NumberOfGames = n(),
              MostCommonResult = getMode(Result)) %>%
    ungroup()
  
  View(df, title='Win Percentage Per Weekday')
}

# Get descriptive statistics for goals scored per weekday ----------------------

get_home_goals_descriptive_weekday <- function(df) {
  goals <- df %>%
    mutate(Weekday = weekdays(Date)) %>%
    group_by(Weekday) %>%
    filter(!is.na(Weekday)) %>%
    summarise(Max = max(HomeGoals, na.rm=TRUE),
              Min = min(HomeGoals, na.rm=TRUE),
              Mean = mean(HomeGoals, na.rm=TRUE),
              StandardDeviation = sd(HomeGoals, na.rm=TRUE),
              SDMeanError = sd(HomeGoals, na.rm=TRUE) / n(),
              Median = quantile(HomeGoals, na.rm=TRUE)[3],
              Mode = getMode(HomeGoals),
              Quantile1 = quantile(HomeGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(HomeGoals, na.rm=TRUE)[4],
              Skewness = skewness(HomeGoals, na.rm=TRUE),
              Kurtosis = kurtosis(HomeGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(HomeGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  
  View(goals,title='Home Goals Per Weekday Statistics')
}

get_away_goals_descriptive_weekday <- function(df) {
  goals <- df %>%
    mutate(Weekday = weekdays(Date)) %>%
    filter(!is.na(Weekday)) %>%
    group_by(Weekday) %>%
    summarise(Max = max(AwayGoals, na.rm=TRUE),
              Min = min(AwayGoals, na.rm=TRUE),
              Mean = mean(AwayGoals, na.rm=TRUE),
              StandardDeviation = sd(AwayGoals, na.rm=TRUE),
              SDMeanError = sd(AwayGoals, na.rm=TRUE) / n(),
              Median = quantile(AwayGoals, na.rm=TRUE)[3],
              Mode = getMode(AwayGoals),
              Quantile1 = quantile(AwayGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(AwayGoals, na.rm=TRUE)[4],
              Skewness = skewness(AwayGoals, na.rm=TRUE),
              Kurtosis = kurtosis(AwayGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(AwayGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  
  View(goals,title='Away Goals Per Weekday Statistics')
}


get_combined_goals_descriptive_weekday <- function(df) {
  goals <- df %>%
    mutate(CombinedGoals = HomeGoals + AwayGoals,
           Weekday = weekdays(Date)) %>%
    filter(!is.na(Weekday)) %>%
    group_by(Weekday) %>%
    summarise(Max = max(CombinedGoals, na.rm=TRUE),
              Min = min(CombinedGoals, na.rm=TRUE),
              Mean = mean(CombinedGoals, na.rm=TRUE),
              StandardDeviation = sd(CombinedGoals, na.rm=TRUE),
              SDMeanError = sd(CombinedGoals, na.rm=TRUE) / n(),
              Median = quantile(CombinedGoals, na.rm=TRUE)[3],
              Mode = getMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(CombinedGoals, na.rm=TRUE)[4],
              Skewness = skewness(CombinedGoals, na.rm=TRUE),
              Kurtosis = kurtosis(CombinedGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(CombinedGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  
  
  View(goals,title='Combined Goals Per Weekday Statistics')
}
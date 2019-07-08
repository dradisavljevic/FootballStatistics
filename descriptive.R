# Function to calculate mode ---------------------------------------------------

getMode <- function(v) {
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
    summarise(Goals = sum(AwayGoals),
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
    summarise(Goals = sum(HomeGoals),
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
    summarise(Max = max(HomeGoals),
              Min = min(HomeGoals),
              Mean = mean(HomeGoals),
              StandardDeviation = sd(HomeGoals),
              SDMeanError = sd(HomeGoals) / n(),
              Median = quantile(HomeGoals)[3],
              Mode = getMode(HomeGoals),
              Quantile1 = quantile(HomeGoals)[2],
              Quantil3 = quantile(HomeGoals)[4],
              Skewness = skewness(HomeGoals),
              Kurtosis = kurtosis(HomeGoals),
              Games = n(),
              Total = sum(HomeGoals),
    ) %>%
    ungroup()
  
  View(goals,title='Home Goals Per League Season Statistics')
}

get_away_goals_descriptive <- function(df) {
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(Max = max(AwayGoals),
              Min = min(AwayGoals),
              Mean = mean(AwayGoals),
              StandardDeviation = sd(AwayGoals),
              SDMeanError = sd(AwayGoals) / n(),
              Median = quantile(AwayGoals)[3],
              Mode = getMode(AwayGoals),
              Quantile1 = quantile(AwayGoals)[2],
              Quantil3 = quantile(AwayGoals)[4],
              Skewness = skewness(AwayGoals),
              Kurtosis = kurtosis(AwayGoals),
              Games = n(),
              Total = sum(AwayGoals),
    ) %>%
    ungroup()
  
  View(goals,title='Away Goals Per League Season Statistics')
}


get_combined_goals_descriptive <- function(df) {
  df <- df %>%
    mutate(CombinedGoals = HomeGoals + AwayGoals)
  
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(Max = max(CombinedGoals),
              Min = min(CombinedGoals),
              Mean = mean(CombinedGoals),
              StandardDeviation = sd(CombinedGoals),
              SDMeanError = sd(CombinedGoals) / n(),
              Median = quantile(CombinedGoals)[3],
              Mode = getMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals)[2],
              Quantil3 = quantile(CombinedGoals)[4],
              Skewness = skewness(CombinedGoals),
              Kurtosis = kurtosis(CombinedGoals),
              Games = n(),
              Total = sum(CombinedGoals),
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
    summarise(Max = max(CombinedGoals),
              Min = min(CombinedGoals),
              Mean = mean(CombinedGoals),
              StandardDeviation = sd(CombinedGoals),
              SDMeanError = sd(CombinedGoals) / n(),
              Median = quantile(CombinedGoals)[3],
              Mode = getMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals)[2],
              Quantil3 = quantile(CombinedGoals)[4],
              Skewness = skewness(CombinedGoals),
              Kurtosis = kurtosis(CombinedGoals),
              Games = n(),
              Total = sum(CombinedGoals),
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
    summarise(Max = max(CombinedGoals),
              Min = min(CombinedGoals),
              Mean = mean(CombinedGoals),
              StandardDeviation = sd(CombinedGoals),
              SDMeanError = sd(CombinedGoals) / n(),
              Median = quantile(CombinedGoals)[3],
              Mode = getMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals)[2],
              Quantil3 = quantile(CombinedGoals)[4],
              Skewness = skewness(CombinedGoals),
              Kurtosis = kurtosis(CombinedGoals),
              Games = n(),
              Total = sum(CombinedGoals),
    ) %>%
    filter(Total == min(Total)) %>%
    arrange(desc(Total), .by_group = FALSE) %>%
    ungroup()
  
  View(goals,title='Lowest Scoring Matchday Statistics')
}

# Percentage of game outcome per league season --------------------------------

get_win_percentage_league <- function(df) {
  df <- df %>%
    group_by(Season, League) %>%
    summarise(PercentageWinsHome = round(sum(Outcome == 'H')/n(), 2),
              PercentageWinsGuest = round(sum(Outcome == 'G')/n(), 2),
              PercentageDraws = round(sum(Outcome == 'D')/n(), 2)) %>%
    ungroup()
  
  View(df, title='Win Percentage Per League Season')
}

# Percentage of game outcome per league matchday -------------------------------

get_win_percentage_league_matchday <- function(df) {
  df <- df %>%
    group_by(Season, League, Matchday) %>%
    summarise(PercentageWinsHome = round(sum(Outcome == 'H')/n(), 2),
              PercentageWinsGuest = round(sum(Outcome == 'G')/n(), 2),
              PercentageDraws = round(sum(Outcome == 'D')/n(), 2)) %>%
    ungroup()
  
  View(df, title='Win Percentage Per League Matchday')
  
}

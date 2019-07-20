GetMode <- function(v) {
  # Calculate mode of values of vectors.
  #
  # Args:
  #  v: vector of values
  #
  # Returns:
  #  Mode of the vector
  uv <- na.omit(v)
  uv <- unique(v)
  mode <- uv[which.max(tabulate(match(v, uv)))]
  return(mode)
}

GetTeamsPerLevel <- function(df) {
  # Print out the number of different teams that played per league level.
  #
  # Args:
  #  df: dataframe containing league information
  numberOfLeagueLevels <- max(df$Level, na.rm = TRUE)
  for (i in seq(1,numberOfLeagueLevels)){
    numberOfTeams <- nrow(table(df$Host[df$Level==i]))
    print(paste("Leagues at level ", i, " had ", numberOfTeams, " different teams playing in it so far."))
  }
}

GetMinMaxGoalsPerTeam <- function(df) {
  # Prints out 10 teams that scored the most and 10 teams that scored the most
  # goals per league season.
  #
  # Args:
  #  df: dataframe containing league information
  goals <- df %>%
    group_by(Season, Guest) %>%
    summarise(Goals = sum(AwayGoals, na.rm=TRUE),
              NumberOfGames = n(),
              Mean = mean(AwayGoals, na.rm=TRUE)) %>%
    ungroup()
  
  goals <- goals[with(goals,order(Goals)),]
  print("Teams that scored least away goals per season are: ")
  print(goals[1:10,])
  print("--------------------------------------------------------")
  print("Teams that scored most away goals per season are: ")
  goals <- goals[with(goals,order(-Goals)),]
  print(goals[1:10,])
  print("--------------------------------------------------------")
  
  goals <- df %>%
    group_by(Season, Host) %>%
    summarise(Goals = sum(HomeGoals, na.rm=TRUE),
              NumberOfGames = n(),
              Mean = mean(HomeGoals, na.rm=TRUE)) %>%
    ungroup()
  
  goals <- goals[with(goals,order(Goals)),]
  print("Teams that scored least home goals per season are: ")
  print(goals[1:10,])
  print("--------------------------------------------------------")
  print("Teams that scored most home goals per season are: ")
  goals <- goals[with(goals,order(-Goals)),]
  print(goals[1:10,])
  print("--------------------------------------------------------")
}

GetClubCountPerCity <- function(df) {
  # Display names of the cities that have more than one football club playing
  # in the league system.
  #
  # Args:
  #  df: dataframe containing league information
  df <- df %>%
    group_by(Host) %>%
    summarise(NumberOfGames = n()) %>%
    ungroup()
  
  suppressWarnings(
    df <- df %>% separate(Host, sep = "\\(", into = c("ClubName", "CityName") )
  )
  
  df$CityName <- gsub("\\)", "", df$CityName)
  
  df <- df %>%
    group_by(CityName) %>%
    summarise(NumberOfClubs = n()) %>%
    ungroup()
  
  df <- df[df$NumberOfClubs > 1,]
  
  df <- df[with(df,order(-NumberOfClubs)),]
  
  View(df, title="Cities with more than one club participating in leagues: ")
  print(df)
}

GetDescriptiveStatsHomeGoals <- function(df) {
  # Display descriptive statistics (max, min, mean, mode, standard deviation,
  # SD mean error, median, first and third quantile, skeweness, kurtosis and 
  # number of games) for home goals per season.
  #
  # Args:
  #  df: dataframe containing league information
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(Max = max(HomeGoals, na.rm=TRUE),
              Min = min(HomeGoals, na.rm=TRUE),
              Mean = mean(HomeGoals, na.rm=TRUE),
              StandardDeviation = sd(HomeGoals, na.rm=TRUE),
              SDMeanError = sd(HomeGoals, na.rm=TRUE) / n(),
              Median = quantile(HomeGoals, na.rm=TRUE)[3],
              Mode = GetMode(HomeGoals),
              Quantile1 = quantile(HomeGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(HomeGoals, na.rm=TRUE)[4],
              Skewness = skewness(HomeGoals, na.rm=TRUE),
              Kurtosis = kurtosis(HomeGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(HomeGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  View(goals,title="Home Goals Per League Season Statistics")
}

GetDescriptiveStatsAwayGoals <- function(df) {
  # Display descriptive statistics (max, min, mean, mode, standard deviation,
  # SD mean error, median, first and third quantile, skeweness, kurtosis and 
  # number of games) for away goals per season.
  #
  # Args:
  #  df: dataframe containing league information
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(Max = max(AwayGoals, na.rm=TRUE),
              Min = min(AwayGoals, na.rm=TRUE),
              Mean = mean(AwayGoals, na.rm=TRUE),
              StandardDeviation = sd(AwayGoals, na.rm=TRUE),
              SDMeanError = sd(AwayGoals, na.rm=TRUE) / n(),
              Median = quantile(AwayGoals, na.rm=TRUE)[3],
              Mode = GetMode(AwayGoals),
              Quantile1 = quantile(AwayGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(AwayGoals, na.rm=TRUE)[4],
              Skewness = skewness(AwayGoals, na.rm=TRUE),
              Kurtosis = kurtosis(AwayGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(AwayGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  View(goals,title="Away Goals Per League Season Statistics")
}

GetDescriptiveStatsCombinedGoals <- function(df) {
  # Display descriptive statistics (max, min, mean, mode, standard deviation,
  # SD mean error, median, first and third quantile, skeweness, kurtosis and 
  # number of games) for combined goals per season.
  #
  # Args:
  #  df: dataframe containing league information
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
              Mode = GetMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(CombinedGoals, na.rm=TRUE)[4],
              Skewness = skewness(CombinedGoals, na.rm=TRUE),
              Kurtosis = kurtosis(CombinedGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(CombinedGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  View(goals,title="Combined Goals Per League Season Statistics")
}

GetMaximumGoalsMatchday <- function(df) {
  # Display descriptive statistics (max, min, mean, mode, standard deviation,
  # SD mean error, median, first and third quantile, skeweness, kurtosis and 
  # number of games) for highest scoring matchdays per season.
  #
  # Args:
  #  df: dataframe containing league information
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
              Mode = GetMode(CombinedGoals),
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
  View(goals,title="Highest Scoring Matchday Statistics")
}

GetMinimumGoalsPerMatchday <- function(df) {
  # Display descriptive statistics (max, min, mean, mode, standard deviation,
  # SD mean error, median, first and third quantile, skeweness, kurtosis and 
  # number of games) for fewest scoring matchdays per season.
  #
  # Args:
  #  df: dataframe containing league information
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
              Mode = GetMode(CombinedGoals),
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
  View(goals,title="Lowest Scoring Matchday Statistics")
}

GetWinPercentagePerLeague <- function(df) {
  # Display win percentage for home and away games, as well as games drawn
  # per league season, as well as the most common result for that season.
  #
  # Args:
  #  df: dataframe containing league information
  df <- df %>%
    mutate(Result = paste(HomeGoals, ":", AwayGoals)) %>%
    group_by(Season, League) %>%
    summarise(PercentageWinsHome = round(sum(Outcome == "H, na.rm=TRUE")/n() * 100, 2),
              PercentageDraws = round(sum(Outcome == "D", na.rm=TRUE)/n() * 100, 2),
              PercentageWinsGuest = round(sum(Outcome == "G", na.rm=TRUE)/n() * 100, 2),
              NumberOfGames = n(),
              MostCommonResult = GetMode(Result)) %>%
    ungroup()
  View(df, title="Win Percentage Per League Season")
}

GetWinPercentagePerLeagueMatchday <- function(df) {
  # Display win percentage for home and away games, as well as games drawn
  # per league seasons matchday, as well as the most common result for that 
  # seasons league matchday.
  #
  # Args:
  #  df: dataframe containing league information
  df <- df %>%
    mutate(Result = paste(HomeGoals, ":", AwayGoals)) %>%
    group_by(Season, League, Matchday) %>%
    summarise(PercentageWinsHome = round(sum(Outcome == "H", na.rm=TRUE)/n() * 100, 2),
              PercentageDraws = round(sum(Outcome == "D", na.rm=TRUE)/n() * 100, 2),
              PercentageWinsGuest = round(sum(Outcome == "G", na.rm=TRUE)/n() * 100, 2),
              NumberOfGames = n(),
              MostCommonResult = GetMode(Result)) %>%
    ungroup()
  View(df, title="Win Percentage Per League Matchday")
}

GetWinPercentagePerDay <- function(df) {
  # Display win percentage for home and away games, as well as games drawn
  # per day of the week, as well as the most common result for that day.
  #
  # Args:
  #  df: dataframe containing league information
  df <- df %>%
    mutate(Weekday = weekdays(Date),
           Result = paste(HomeGoals, ":", AwayGoals)) %>%
    filter(!is.na(Weekday)) %>%
    group_by(Weekday) %>%
    summarise(PercentageWinsHome = round(sum(Outcome == "H", na.rm=TRUE)/n() * 100, 2),
              PercentageDraws = round(sum(Outcome == "D", na.rm=TRUE)/n() * 100, 2),
              PercentageWinsGuest = round(sum(Outcome == "G", na.rm=TRUE)/n() * 100, 2),
              NumberOfGames = n(),
              MostCommonResult = GetMode(Result)) %>%
    ungroup()
  View(df, title="Win Percentage Per Weekday")
}

GetDescritiveStatsDayHome <- function(df) {
  # Display descriptive statistics (max, min, mean, mode, standard deviation,
  # SD mean error, median, first and third quantile, skeweness, kurtosis and 
  # number of games) for home goals on each day of the week.
  #
  # Args:
  #  df: dataframe containing league information
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
              Mode = GetMode(HomeGoals),
              Quantile1 = quantile(HomeGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(HomeGoals, na.rm=TRUE)[4],
              Skewness = skewness(HomeGoals, na.rm=TRUE),
              Kurtosis = kurtosis(HomeGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(HomeGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  View(goals,title="Home Goals Per Weekday Statistics")
}

GetDescritiveStatsDayAway <- function(df) {
  # Display descriptive statistics (max, min, mean, mode, standard deviation,
  # SD mean error, median, first and third quantile, skeweness, kurtosis and 
  # number of games) for away goals on each day of the week.
  #
  # Args:
  #  df: dataframe containing league information
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
              Mode = GetMode(AwayGoals),
              Quantile1 = quantile(AwayGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(AwayGoals, na.rm=TRUE)[4],
              Skewness = skewness(AwayGoals, na.rm=TRUE),
              Kurtosis = kurtosis(AwayGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(AwayGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  View(goals,title="Away Goals Per Weekday Statistics")
}

GetDescritiveStatsDayCombined <- function(df) {
  # Display descriptive statistics (max, min, mean, mode, standard deviation,
  # SD mean error, median, first and third quantile, skeweness, kurtosis and 
  # number of games) for combined goals on each day of the week.
  #
  # Args:
  #  df: dataframe containing league information
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
              Mode = GetMode(CombinedGoals),
              Quantile1 = quantile(CombinedGoals, na.rm=TRUE)[2],
              Quantile3 = quantile(CombinedGoals, na.rm=TRUE)[4],
              Skewness = skewness(CombinedGoals, na.rm=TRUE),
              Kurtosis = kurtosis(CombinedGoals, na.rm=TRUE),
              Games = n(),
              Total = sum(CombinedGoals, na.rm=TRUE),
    ) %>%
    ungroup()
  View(goals,title="Combined Goals Per Weekday Statistics")
}

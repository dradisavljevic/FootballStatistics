# Get number of different teams that have played in the league -----------------

get_teams_per_level <- function(df) {
  number_of_levels <- max(df$Level, na.rm = TRUE)
  for (i in seq(1,number_of_levels)){
    team_number <- nrow(table(df$Host[df$Level==i]))
    print(paste('Leagues at level ', i, ' had ', team_number, ' different teams playing in it so far.'))
  }
}

# Get minimum and maxmum of goals per league by season --------------------------

get_min_max_goals_league <- function(df) {
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(Goals = sum(HomeGoals, AwayGoals),
              NumberOfGames = n(),
              MeanHome = mean(HomeGoals, na.rm=TRUE),
              MeanAway = mean(AwayGoals, na.rm=TRUE)) %>%
    ungroup()
  
  goals <- goals[goals$Goals > 100,]
  
  goals <- goals[with(goals,order(Goals)),]
  print('League seasons with least scored goals are: ')
  print(goals[1:5,])
  print('--------------------------------------------------------')
  print('League seasons with most scored goals are: ')
  goals <- goals[with(goals,order(-Goals)),]
  print(goals[1:5,])
  print('--------------------------------------------------------')
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

# Get maximum number of goals scored per league season -------------------------

get_max_goals_statistics <- function(df) {
  df <- df %>%
    mutate(CombinedGoals = HomeGoals + AwayGoals)
  
  goals <- df %>%
    group_by(Season, League) %>%
    summarise(MaximumGoalsHome = max(HomeGoals),
              MaximumGoalsAway = max(AwayGoals),
              MaximumGoalsCombined = max(CombinedGoals)) %>%
    ungroup()
  
  View(goals,title='Maximum amount of goals per league season')
}

# Get maximum number of goals scored per league matchday -----------------------

get_goal_max_per_matchday <- function(df) {
  df <- df %>%
    mutate(CombinedGoals = HomeGoals + AwayGoals)
  
  goals <- df %>%
    group_by(Season, League, Matchday) %>%
    summarise(AmountOfGoalsPerDay = sum(CombinedGoals)) %>%
    filter(AmountOfGoalsPerDay == max(AmountOfGoalsPerDay)) %>%
    arrange(desc(AmountOfGoalsPerDay), .by_group = FALSE) %>%
    ungroup()
  
  View(goals,title='Amount of Goals Per Matchday')
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

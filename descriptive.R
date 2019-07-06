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
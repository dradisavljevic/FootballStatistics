# Function that gets opening weekend results ------------------------------------

get_opening_weekend <- function(df) {
  opening_games <- df %>%
    group_by(Season, Team) %>%
    mutate(FinalPoints = max(Points), 
           FinalGoalsFor = sum(FTGF), 
           FinalGoalsAgainst = sum(FTGA),
           Game = rank(Date)) %>%
    ungroup() %>%
    group_by(Season, Game) %>%
    mutate(TablePosition = n() + 1 - min_rank(FinalPoints)) %>%
    ungroup() %>%
    filter(Game == 1) %>%
    group_by(Team) %>%
    mutate(FinalPointsLast = lag(FinalPoints),
           FinalGoalsForLast = lag(FinalGoalsFor),
           FinalGoalsAgainstLast = lag(FinalGoalsAgainst),
           FinalTablePositionLast = lag(TablePosition)) %>%
    ungroup()
  
  return(opening_games)
}

# Calculate Advantage of The Home Field For Opening Weekend --------------------

get_opening_weekend_home_adv <- function(opening_games) {
  opening_home_adv <- opening_games %>%
    group_by(Venue) %>%
    summarise(PercentageWin = round((sum(Outcome == 'W')/n()) * 100, 2), 
              PercentageLoss = round((sum(Outcome == 'L')/n()) * 100, 2),
              PercentageDraw = round((sum(Outcome == 'D')/n()) * 100, 2))
  
  return(opening_home_adv)
}

# Calculate Home Field Advantage Throughout Whole Season ------------------------

get_home_adv_season <- function(df) {
  home_adv <- df %>%
    group_by(Venue) %>%
    summarise(PercentageWin = round((sum(Outcome == 'W')/n()) * 100, 2), 
              PercentageLoss = round((sum(Outcome == 'L')/n()) * 100, 2),
              PercentageDraw = round((sum(Outcome == 'D')/n()) * 100, 2))
  
  return(home_adv)
}

# Get Opening Weekend Win Percentage Per Season ---------------------------------

get_seasonal_opening_percentage <- function(opening_games) {
  opening_games %>%
    group_by(Season, Venue) %>%
    summarise(PercentageWin = round((sum(Outcome == 'W')/n()) * 100, 2), 
              PercentageLoss = round((sum(Outcome == 'L')/n()) * 100, 2),
              PercentageDraw = round((sum(Outcome == 'D')/n()) * 100, 2)) %>%
    gather(key = "Result", value = 'Percentage', PercentageWin:PercentageDraw) %>%
    mutate(Result = case_when(Result == 'PercentageWin' ~ 'Winning Percentage',
                              Result == 'PercentageLoss' ~ 'Losing Percentage',
                              TRUE ~ 'Percent Draws')) %>%
    filter(Venue == 'Home') %>%
    ggplot(aes(Season, Percentage, color = Result)) +
    geom_point() +
    geom_line(size = 1) +
    theme_tq() +
    scale_color_tq() +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust = 1, 
                                     size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_blank()) +
    labs(x = 'Year',
         y = 'Percentage of Results by Year',
         title = 'The Percent of Results (Win, Draw, Loss) per Season\nfor Home Teams on Opening Weekend')
}

# Function That Gets Home Advantage Per Team ------------------------------------

get_home_adv_team <- function(opening_games) {
  home_adv_team <- opening_games %>%
    group_by(Team) %>%
    mutate(NumberOfSeasons = n()) %>%
    ungroup() %>%
    filter(NumberOfSeasons >= max(NumberOfSeasons)/2) %>% 
    group_by(Team) %>%
    select(-NumberOfSeasons) %>%
    ungroup() %>%
    group_by(Team, Venue) %>%
    summarise(WinningPercentage = round((sum(Outcome == 'W')/n()) * 100, 2), 
              TotalNumberOfGames = n()) %>%
    ungroup()
  
  return(home_adv_team)
}

# Calculate Winning Percentage For Both Home And Away Games on Opening Weekend --

get_home_away_win_percent <- function(team_advantage) {
  team_advantage %>%
    arrange(-WinningPercentage) %>%
    mutate(Team = factor(Team, unique(Team)),
           Venue = factor(Venue, levels = c('Home', 'Away'))) %>%
    ggplot(aes(Team, WinningPercentage, fill = Venue)) +
    facet_wrap(~Venue, scales = 'fixed', ncol = 1) +
    geom_bar(stat = 'identity') +
    theme_tq() +
    scale_fill_tq() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 12),
          legend.position = 'none') +
    geom_text(aes(x = Team, y = WinningPercentage + 5, label = paste0('n=', as.character(TotalNumberOfGames)))) +
    labs(x = '',
         y = 'Winning Percentage',
         title = 'Winning Percentage for Teams on Opening Weekend both Home and Away',
         subtitle = 'Only Teams Participating in more than Half of the Seasons were included')
}

# Calculate Overal Win Percentage on Opening Weekend --------------------------

get_overall_win_percent <- function(opening_games) {
  opening_games %>%
    group_by(Team) %>%
    mutate(NumberOfSeasons = n()) %>%
    ungroup() %>%
    filter(NumberOfSeasons >= max(NumberOfSeasons)/2) %>% 
    group_by(Team) %>%
    select(-NumberOfSeasons) %>%
    ungroup() %>%
    group_by(Team) %>%
    summarise(WinningPercentage = round((sum(Outcome == 'W')/n()) * 100, 2), 
              TotalNumberOfGames = n()) %>%
    ungroup() %>%
    arrange(-WinningPercentage) %>%
    mutate(Team = factor(Team, unique(Team))) %>%
    ggplot(aes(Team, WinningPercentage)) +
    geom_bar(stat = 'identity', fill = 'red', color = 'black') +
    theme_tq() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 12),
          legend.position = 'none') +
    geom_text(aes(x = Team, y = WinningPercentage + 5, label = paste0('n=', as.character(TotalNumberOfGames)))) +
    labs(x = '',
         y = 'Winning Percentage',
         title = 'Winning Percentage for Teams on Opening Weekend',
         subtitle = 'Only Teams Participating in more than Half of the Seasons were included')
}

# Get seasonal data from desired league ----------------------------------------

seasonal_result_data <- function(data) {
  seasonal_data <- data %>%
    gather(key = 'Venue', value = Team, Host:Guest) %>% 
    arrange(Date) %>%
    mutate_if(is.factor, as.character) %>%
    mutate(Venue = ifelse(Venue == 'Host',
                          'Home',
                          'Away'),
           Outcome = case_when(Venue == 'Home' & Outcome == 'H' ~ 'W',
                               Venue == 'Home' & Outcome == 'G' ~ 'L',
                               Venue == 'Away' & Outcome == 'H' ~ 'L',
                               Venue == 'Away' & Outcome == 'G' ~ 'W',
                               TRUE ~ Outcome),
           FTGF = ifelse(Venue == 'Home', HomeGoals, AwayGoals),  #Full Time Goals For
           FTGA = ifelse(Venue == 'Home', AwayGoals, HomeGoals),  #Full Time Goals Against
           GoalDifference = FTGF - FTGA,
           PointsEarned = case_when(Outcome == 'W' ~ 3,
                                    Outcome == 'D' ~ 1,
                                    Outcome == 'L' ~ 0)) %>% 
    select(League, Season, Date, Team, Venue, Outcome, FTGF, 
           FTGA, GoalDifference, PointsEarned) %>%
    group_by(Season, League, Team) %>%
    mutate(Points = cumsum(PointsEarned),
           GoalDifferenceTotal = cumsum(GoalDifference)) %>% #calculating the number of points each team has through out the season
    ungroup()
  
  return (seasonal_data)
}


# Get Seasonal Performance for club --------------------------------------------

get_club_seasonal_performance <- function(df, club_name) {
  df %>% 
    filter(Team == club_name) %>%
    ggplot(aes(Date, Points)) +
    facet_wrap(~Season, scales = 'free') +
    geom_line() +
    theme_tq() +
    scale_x_date(date_breaks = '2 month', date_labels = '%m/%d') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                     vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 12),
          axis.title = element_text(size = 12)) +
    labs(x = '', 
         y = 'Point Tally',
         title =  paste(club_name, 
                        'Point Tally over the course of each season from\nthe first season to the 2018/2019 season'))
}

# Get Season Ending Table -------------------------------------------------------

get_season_ending_table <- function(df) {
  season_ending <- df %>%
    group_by(Season, Team) %>%
    summarise(FinalPoints = max(Points), 
              FinalGoalsFor = sum(FTGF), 
              FinalGoalsAgainst = sum(FTGA),
              FinalGoalDifference = sum(GoalDifference)) %>%
    ungroup() %>%
    group_by(Season) %>%
    mutate(TablePosition = n() + 1 - rank(FinalPoints, ties.method = 'min')) %>%
    ungroup()
  
  return(season_ending)
}

# Get Season Ending Statistics --------------------------------------------------

get_season_ending_stat <- function(df) {
  season_ending_stats <- df %>%
    group_by(Team) %>%
    summarise(AVGFinalPoints = round(mean(FinalPoints), 0), 
              SDFinalPoints = round(sd(FinalPoints), 0), 
              AVGFinalGoalsFor = round(mean(FinalGoalsFor), 0), 
              SDFinalGoalsFor = round(sd(FinalGoalsFor), 0),
              AVGFinalGoalsAgainst = round(mean(FinalGoalsAgainst), 0), 
              SDFinalGoalsAgaints = round(sd(FinalGoalsAgainst), 0),
              AVGTablePosition = round(mean(TablePosition), 0),
              SDTablePosition = round(sd(TablePosition), 0),
              NumberOfSeasons = n())
  
  return(season_ending_stats)
}

# Plot Average Points For Teams -------------------------------------------------

plot_season_ending <- function(df) {
  df %>%
    filter(NumberOfSeasons >= max(NumberOfSeasons)/2) %>%
    arrange(-AVGFinalPoints) %>%
    mutate(Team = factor(Team, Team)) %>%
    ggplot(aes(Team, AVGFinalPoints)) +
    geom_bar(stat = 'identity', fill = 'red') +
    geom_point(color = 'navy') +
    geom_errorbar(aes(ymin = AVGFinalPoints - 2*SDFinalPoints, 
                      ymax = AVGFinalPoints + 2*SDFinalPoints), 
                  color = 'navy', size = 1) +
    theme_tq() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                     vjust = 0.5, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12)) +
    labs(x = 'Team',
         y = 'Final Points',
         title = 'Average Final Points from first season to 2018/2019 season',
         subtitle = 'Only teams that have participated in more than half of seasons are shown')
  
}

# Function that plots final points using joyplot -------------------------------

joyplot_points <- function(df) {
  df %>%
    group_by(Team) %>%
    mutate(NumberOfSeasons = n(),
           AVGFinalPoints = round(mean(FinalPoints), 0)) %>%
    ungroup() %>%
    filter(NumberOfSeasons >= max(NumberOfSeasons)/2) %>%
    arrange(AVGFinalPoints) %>%
    mutate(Team = factor(Team, unique(Team))) %>%
    ggplot(aes(FinalPoints, Team)) +
    geom_joy(scale = 0.9, rel_min_height = 0.01, 
             fill = 'red', color = 'black', size = 1) +
    theme_tq()+
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12)) +
    labs(x = 'Final Points',
         y = 'Team',
         title = 'Joyplot showing teams\' individual season final point tally distribution')
}



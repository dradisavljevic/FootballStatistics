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

# Merge city columns with club name ones ---------------------------------------

merge_name_and_city <- function(df) {
  team_id_vector <- c()
  team_name_vector <- c()
  
  by(df, 1:nrow(df), function(row) {
    if (!(row$HostID) %in% team_id_vector){
      team_id_vector[length(team_id_vector)+1] <<- row$HostID
      team_name_vector[length(team_name_vector)+1] <<-
        paste(row$Host, ' (',row$HostCity, ')', sep='')
    }
  })
  
  for (i in seq(1,length(team_id_vector))){
    df$Host <- ifelse(df$HostID==team_id_vector[i],
                               as.character(team_name_vector[i]),
                               as.character(df$Host))
    df$Guest <- ifelse(df$GuestID==team_id_vector[i],
                                as.character(team_name_vector[i]),
                                as.character(df$Guest))
  }
  
  return (df)
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

# Replace missing dates function ----------------------------------------------

replace_missing_dates <- function(df) {
  rows_with_missing_values <- df[is.na(df$Date),]
  missing_dates <- c()
  
  for (i in seq(1,nrow(rows_with_missing_values))){
    missing_dates[i] <- df$Date[!is.na(df$Date) 
                                         & df$Season == rows_with_missing_values$Season[i] 
                                         & df$Matchday == rows_with_missing_values$Matchday[i] 
                                         & df$Level == rows_with_missing_values$Level[i]][1]
  }
  
  for (i in seq(1,length(missing_dates))){
    df$Date[is.na(df$Date)][1] <- as.Date(missing_dates[i], origin=lubridate::origin)
  }
  
  rows_with_missing_values <- df[is.na(df$Date),]
  
  if (dim(rows_with_missing_values)[1] != 0) {
    matchday <- rows_with_missing_values$Matchday[1]-1
    missing_date <- df$Date[!is.na(df$Date) 
                            & df$Season == rows_with_missing_values$Season[1] 
                            & df$Matchday+1 == rows_with_missing_values$Matchday[1] 
                            & df$Level == rows_with_missing_values$Level[1]][1]
  
    iterations <- dim(rows_with_missing_values)[1]
    
    for (i in seq(1,iterations)){
      df$Date[is.na(df$Date)][1] <- as.Date(missing_date, origin=lubridate::origin)+7*(df$Matchday[is.na(df$Date)][1]-matchday)
    }
    
    }
  
  return(df)
  
}

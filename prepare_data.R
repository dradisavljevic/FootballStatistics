GetDateFromString <- function(date) {
  # Convert date from string to a YYYY-MM-DD format.
  #
  # Args:
  #  date: string value of the date to be processed
  #
  # Returns:
  #  Date object in correct format
  date <- gsub("\\.", "/", date)
  date <- dmy(date)
  return(date)
}

CapitalizeWords <- function(words) {
  # Converts string to one where every word is capitalized.
  # Equivalent to python's title() function.
  #
  # Args:
  #  words: string to be capitalized
  #
  # Returns:
  #  String with all words capitalized
  words <- as.character(words)
  capitalizedWords <- strsplit(words, " ")[[1]]
  capitalizedWords <- paste(toupper(substring(capitalizedWords, 1,1)),
                            substring(capitalizedWords, 2), 
                            sep="", collapse=" ")
  return(capitalizedWords)
}

MergeClubAndCityNames <- function(df) {
  # Merges names of the football clubs with the name of cities that they
  # play in. This is in order to avoid bundling clubs with same names, but
  # from different cities together.
  #
  # Args:
  #  df: dataframe containing seasonal league information
  #
  # Returns:
  #  Dataframe with club name columns changed to the format of ClubName (City)
  teamIDVector <- c()
  teamNameVector <- c()
  
  by(df, 1:nrow(df), function(row) {
    if (!(row$HostID) %in% teamIDVector){
      teamIDVector[length(teamIDVector)+1] <<- row$HostID
      teamNameVector[length(teamNameVector)+1] <<-
        paste(row$Host, " (",row$HostCity, ")", sep="")
    }
  })
  
  for (i in seq(1,length(teamIDVector))){
    df$Host <- ifelse(df$HostID==teamIDVector[i],
                               as.character(teamNameVector[i]),
                               as.character(df$Host))
    df$Guest <- ifelse(df$GuestID==teamIDVector[i],
                                as.character(teamNameVector[i]),
                                as.character(df$Guest))
  }
  return (df)
}

RemoveWhiteSpace <- function(df) {
  # Removes trailing and leading whitespace from the football 
  # club name and league name columns.
  #
  # Args:
  #   df: dataframe containing seasonal league information
  #
  # Returns:
  #   Dataframe with formated columns containing no extra whitespace
  df$Host <- trimws(gsub("\\s+", " ", df$Host),
                    "both", whitespace = "[ \t\r\n]")
  
  df$Guest <- trimws(gsub("\\s+", " ", df$Guest),
                     "both", whitespace = "[ \t\r\n]")
  
  df$League <- trimws(gsub("\\s+", " ", df$League),
                      "both", whitespace = "[ \t\r\n]")
  return (df)
}

RoundUpSeason <- function(df) {
  # Rounds up the season to the end year, changing the column format from
  # YYYY-YYYY to YYYY.
  #
  # Args:
  #  df: dataframe containing seasonal league information
  #
  # Returns:
  #  Dataframe with season column changed
  df$Season <- as.numeric(substr(df$Season, 6, 9))
  return (df)
}

ReplaceMissingDates <- function(df) {
  # Replaces the missing date values from the dataframe by replacing them with
  # assumed new values. Assumption is that every matchday is played exactly 7 
  # days later than that which came before it.
  #
  # Args:
  #  df: dataframe containing seasonal league information
  #
  # Returns:
  #  Dataframe with date column filled in
  missingValuesRow <- df[is.na(df$Date),]
  missingDates <- c()
  
  for (i in seq(1,nrow(missingValuesRow))){
    missingDates[i] <- df$Date[!is.na(df$Date) 
                                         & df$Season == missingValuesRow$Season[i] 
                                         & df$Matchday == missingValuesRow$Matchday[i] 
                                         & df$Level == missingValuesRow$Level[i]][1]
  }
  
  for (i in seq(1,length(missingDates))){
    df$Date[is.na(df$Date)][1] <- as.Date(missingDates[i], origin=lubridate::origin)
  }
  
  missingValuesRow <- df[is.na(df$Date),]
  
  if (dim(missingValuesRow)[1] != 0) {
    matchday <- missingValuesRow$Matchday[1]-1
    missingDate <- df$Date[!is.na(df$Date) 
                            & df$Season == missingValuesRow$Season[1] 
                            & df$Matchday+1 == missingValuesRow$Matchday[1] 
                            & df$Level == missingValuesRow$Level[1]][1]
  
    iterations <- dim(missingValuesRow)[1]
    
    for (i in seq(1,iterations)){
      df$Date[is.na(df$Date)][1] <- as.Date(missingDate, origin=lubridate::origin)+7*(df$Matchday[is.na(df$Date)][1]-matchday)
    }
  }
  return(df)
}

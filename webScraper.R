library(rvest)

## %>% is used to pass object on left hand side as first object on right hand side
## selectorgadget shows CSS selectors from bookmarklet on any website you want

#vignette('selectorgadget')

## First create function that pulls data for the previous day

## Pull team names, and combine names with abbrevations found in boxscores on basketball-reference.com

url <- read_html('http://www.basketball-reference.com/teams/')

teams <- as.data.frame(url %>%
  html_nodes("#active a") %>%
  html_text())

## Abbreviations manually pulled from basketball-reference urls

teams$abbrev <- c('0ATL',
                  '0BOS',
                  '0BRK',
                  '0CHO',
                  '0CHI',
                  '0CLE',
                  '0DAL',
                  '0DEN',
                  '0DET',
                  '0GSW',
                  '0HOU',
                  '0IND',
                  '0LAC',
                  '0LAL',
                  '0MEM',
                  '0MIA',
                  '0MIL',
                  '0MIN',
                  '0NOP',
                  '0NYK',
                  '0OKC',
                  '0ORL',
                  '0PHI',
                  '0PHO',
                  '0POR',
                  '0SAC',
                  '0SAS',
                  '0TOR',
                  '0UTA',
                  '0WAS')

colnames(teams) <- c('team', 'abbrev')

## Find home teams for a given day

## Put year, month & day in character format
## Note - individual box scores have different date format than overall page, hence 2 forms of Month/Day

yesterday <- Sys.Date()-1
yesterdayYear <- substr(as.character(Sys.Date() - 1), 0, 4)
yesterdayMonthGame <- substr(as.character(Sys.Date() - 1), 6, 7)
yesterdayMonth <- gsub('^0', '',substr(as.character(Sys.Date() - 1), 6, 7))
yesterdayDayGame <- substr(as.character(Sys.Date() - 1), 9, 10)
yesterdayDay <- gsub('^0', '',substr(as.character(Sys.Date() - 1), 9, 10))

boxDayUrl <- read_html(paste0('http://www.basketball-reference.com/boxscores/index.cgi?month=',
                  yesterdayMonth,
                  '&day=',
                   yesterdayDay,
                   '&year=',
                   yesterdayYear))

## Pull just the teams playing on a given day

teamsPlaying <- boxDayUrl %>%
                         html_nodes(".wide_table tr:nth-child(1) .wide_table td:nth-child(1) a") %>%
                         html_text()
teamsPlaying <- data.frame(matrix(teamsPlaying, nrow = length(teamsPlaying)/2, byrow=T))
colnames(teamsPlaying) <- c('awayTeam','homeTeam')

## Merge Away & Home teams with respective abbreviations - used later

awayTeams <- merge(teamsPlaying,teams, by = 'team', by.x = 'awayTeam', by.y = 'team')
homeTeams <- merge(teamsPlaying,teams, by = 'team', by.x = 'homeTeam', by.y = 'team')

## Probably unnecessary

homeTeamsList <- list(homeTeams[,3])
awayTeamsList <- list(awayTeams[,3])

## Empty lists to create lists of data frames out of

boxUrlList <- vector("list", length(homeTeamsList))
boxUrlList2 <- vector('list', length(homeTeamsList))
teamStatsGame <- vector('list', length(homeTeamsList))
homeStats <- vector('list', length(homeTeamsList))
homeStatsAdv <- vector('list', length(homeTeamsList))
homeStatsTeam <- list()
homeStatsTeamAdv <- list()
awayStats <- vector('list', length(awayTeamsList))
awayStatsAdv <- vector('list', length(awayTeamsList))
awayStatsTeam <- list()
awayStatsTeamAdv <- list()

## For loop to pull information about all teams that played yesterday

for (i in 1:nrow(homeTeams)) {
  boxUrlList[[i]] <- paste0('http://www.basketball-reference.com/boxscores/',
                                 yesterdayYear, 
                                 yesterdayMonthGame, 
                                 yesterdayDayGame,
                                 homeTeamsList[[1]][[i]],
                                 '.html')
  boxUrlList2[[i]] <- read_html(boxUrlList[[i]])
  
  ## Go through each game one by one & pull out player & team stats for home and away teams
  
  for (i in 1:length(boxUrlList2)) {
    teamStatsGame[[i]] <- as.data.frame(matrix(boxUrlList2[[i]] %>%
                                        html_nodes('#page_content .large_text a') %>%
                                        html_text(),nrow=1))  
    teamsStatsDF <- as.data.frame(teamStatsGame[[i]])
    colnames(teamsStatsDF) <- c('awayTeam', 'homeTeam')
    getAwayCode <- merge(awayTeams, teamsStatsDF, by = 'awayTeam') 
    awayCode <- gsub('^0', '#', getAwayCode$abbrev)
    getHomeCode <- merge(homeTeams, teamsStatsDF, by = 'homeTeam')
    homeCode <- gsub('^0', '#', getHomeCode$abbrev)
    
    ###############  HOME PLAYER STATS ##############
    
    ## Used to pull data from webpage - needs to be in this format for the list of players
    
    text <- paste0(homeCode,"_basic tbody td:nth-child(1)")

    homePlayers <- boxUrlList2[[i]] %>%
      html_nodes(text) %>%
      html_text()
    
    ## Used to pull column names
    
    cols <- boxUrlList2[[i]] %>%
      html_nodes(paste0(homeCode,"_basic .tooltip")) %>%
      html_text()
    
    ## Used to pull home player stats
    
    homePlayerStats <- boxUrlList2[[i]] %>%
      html_nodes(paste0(homeCode,'_basic tbody td:nth-child(2), ',homeCode,'_basic tbody td:nth-child(3), ',
                        homeCode,'_basic tbody td:nth-child(4), ',homeCode,'_basic tbody td:nth-child(5), ',
                        homeCode,'_basic tbody td:nth-child(6), ',homeCode,'_basic tbody td:nth-child(7), ',
                        homeCode,'_basic tbody td:nth-child(8), ',homeCode,'_basic tbody td:nth-child(9), ',
                        homeCode,'_basic tbody td:nth-child(10), ',homeCode,'_basic tbody td:nth-child(11), ',
                        homeCode,'_basic tbody td:nth-child(12), ',homeCode,'_basic tbody td:nth-child(13), ',
                        homeCode,'_basic tbody td:nth-child(14), ',homeCode,'_basic tbody td:nth-child(15), ',
                        homeCode,'_basic tbody td:nth-child(16), ',homeCode,'_basic tbody td:nth-child(17), ',
                        homeCode,'_basic tbody td:nth-child(18), ',homeCode,'_basic tbody td:nth-child(19), ',
                        homeCode,'_basic tbody td:nth-child(20), ',homeCode,"_basic tbody td:nth-child(21)")) %>%
      html_text()
    
    ## Remove missings with 0, and drop "did not play" text 
    ## Then create a dataframe for all possible data - note players that did not play must be addressed next
    
    homePlayerStats <- replace(homePlayerStats, homePlayerStats == "", "0")
    homePlayerStats <- homePlayerStats[!homePlayerStats %in% "Did Not Play"]
    homePlayerStats <- data.frame(matrix(homePlayerStats, 
                                     ncol = 20, 
                                     nrow = (length(homePlayerStats) / 20), 
                                     byrow = T),
                              stringsAsFactors = F)
    
    ## Created to add 0s across all stats for players who did not play in a certain game
    
    zeroRow <- as.character(rep(0, 20))
    
    ## IF there are players who didn't play, add zeros (using zeroRow) for every stat
    ## Necessary because basketball-reference has a cross-column text field saying "did not play"
    
    if(length(homePlayers) > nrow(homePlayerStats)) {
      for (j in (nrow(homePlayerStats)+1): length(homePlayers)) {
        homePlayerStats <- rbind(homePlayerStats, zeroRow)
      }
    } 
    
    ## Merge player information with their statistics, then add in team code & date of the game
    
    homePlayerStatsFull <- data.frame(homePlayers, homePlayerStats)
    colnames(homePlayerStatsFull) <- cols
    homePlayerStatsFull$team <- homeCode
    homePlayerStatsFull$opponent <- awayCode
    homePlayerStatsFull$date <- yesterday
    
    ############### HOME ADVANCED PLAYER STATS ###################
    
    ## Same process, but for advanced instead of basic stats ##
    
    colsAdv <- boxUrlList2[[i]] %>%
      html_nodes(paste0(homeCode,"_advanced .tooltip")) %>%
      html_text()
    
    homePlayerStatsAdv <- boxUrlList2[[i]] %>%
      html_nodes(paste0(homeCode,'_advanced tbody td:nth-child(2), ',homeCode,'_advanced tbody td:nth-child(3), ',
                        homeCode,'_advanced tbody td:nth-child(4), ',homeCode,'_advanced tbody td:nth-child(5), ',
                        homeCode,'_advanced tbody td:nth-child(6), ',homeCode,'_advanced tbody td:nth-child(7), ',
                        homeCode,'_advanced tbody td:nth-child(8), ',homeCode,'_advanced tbody td:nth-child(9), ',
                        homeCode,'_advanced tbody td:nth-child(10), ',homeCode,'_advanced tbody td:nth-child(11), ',
                        homeCode,'_advanced tbody td:nth-child(12), ',homeCode,'_advanced tbody td:nth-child(13), ',
                        homeCode,'_advanced tbody td:nth-child(14), ',homeCode,'_advanced tbody td:nth-child(15), ',
                        homeCode,'_advanced tbody td:nth-child(16), ',homeCode,'_advanced tbody td:nth-child(17), ',
                        homeCode,'_advanced tbody td:nth-child(18), ',homeCode,'_advanced tbody td:nth-child(19), ',
                        homeCode,'_advanced tbody td:nth-child(20), ',homeCode,"_advanced tbody td:nth-child(21)")) %>%
      html_text()
    
    ## Remove missings with 0, and drop "did not play" text 
    ## Then create a dataframe for all possible data - note players that did not play must be addressed next
    
    homePlayerStatsAdv <- replace(homePlayerStatsAdv, homePlayerStatsAdv == "", "0")
    homePlayerStatsAdv <- homePlayerStatsAdv[!homePlayerStatsAdv %in% "Did Not Play"]
    homePlayerStatsAdv <- data.frame(matrix(homePlayerStatsAdv, 
                                            ncol = 15, 
                                            nrow = (length(homePlayerStatsAdv) / 15), ## Note the change here
                                            byrow = T),
                                     stringsAsFactors = F)
    
    
    ## IF there are players who didn't play, add zeros (using zeroRow) for every stat
    ## Necessary because basketball-reference has a cross-column text field saying "did not play"
    
    if(length(homePlayers) > nrow(homePlayerStatsAdv)) {
      for (j in (nrow(homePlayerStatsAdv)+1): length(homePlayers)) {
        homePlayerStatsAdv <- rbind(homePlayerStatsAdv, zeroRow)
      }
    } 
    
    ## Merge player information with their statistics, then add in team code & date of the game
    
    homePlayerStatsAdvFull <- data.frame(homePlayers, homePlayerStatsAdv)
    colnames(homePlayerStatsAdvFull) <- colsAdv
    homePlayerStatsAdvFull$team <- homeCode
    homePlayerStatsAdvFull$opponent <- awayCode
    homePlayerStatsAdvFull$date <- yesterday
    
    ############# HOME TEAM STATS ####################
    
    homeTeamStats <- boxUrlList2[[i]] %>%
      html_nodes(paste0(homeCode,'_basic .stat_total td:nth-child(2), ',homeCode,'_basic .stat_total td:nth-child(3), ',
                        homeCode,'_basic .stat_total td:nth-child(4), ',homeCode,'_basic .stat_total td:nth-child(5), ',
                        homeCode,'_basic .stat_total td:nth-child(6), ',homeCode,'_basic .stat_total td:nth-child(7), ',
                        homeCode,'_basic .stat_total td:nth-child(8), ',homeCode,'_basic .stat_total td:nth-child(9), ',
                        homeCode,'_basic .stat_total td:nth-child(10), ',homeCode,'_basic .stat_total td:nth-child(11), ',
                        homeCode,'_basic .stat_total td:nth-child(12), ',homeCode,'_basic .stat_total td:nth-child(13), ',
                        homeCode,'_basic .stat_total td:nth-child(14), ',homeCode,'_basic .stat_total td:nth-child(15), ',
                        homeCode,'_basic .stat_total td:nth-child(16), ',homeCode,'_basic .stat_total td:nth-child(17), ',
                        homeCode,'_basic .stat_total td:nth-child(18), ',homeCode,'_basic .stat_total td:nth-child(19), ',
                        homeCode,'_basic .stat_total td:nth-child(20), ',homeCode,"_basic .stat_total td:nth-child(21)")) %>%
      html_text()
    
    ## Replace missing with 0s
    
    homeTeamStats <- replace(homeTeamStats, homeTeamStats == "", "0")
    
    ## Merge in team code with stats, then add date and opponent
    
    homeTeamStatsFull <- data.frame(matrix(homeTeamStats, nrow=1))
    colnames(homeTeamStatsFull) <- cols[2:length(cols)]
    homeTeamStatsFull$team <- homeCode
    homeTeamStatsFull$opponent <- awayCode
    homeTeamStatsFull$date <- yesterday
    
    ############# HOME ADVANCED TEAM STATS ####################
    
    homeTeamStatsAdv <- boxUrlList2[[i]] %>%
      html_nodes(paste0(homeCode,'_basic .stat_total td:nth-child(2), ',homeCode,'_basic .stat_total td:nth-child(3), ',
                        homeCode,'_basic .stat_total td:nth-child(4), ',homeCode,'_basic .stat_total td:nth-child(5), ',
                        homeCode,'_basic .stat_total td:nth-child(6), ',homeCode,'_basic .stat_total td:nth-child(7), ',
                        homeCode,'_basic .stat_total td:nth-child(8), ',homeCode,'_basic .stat_total td:nth-child(9), ',
                        homeCode,'_basic .stat_total td:nth-child(10), ',homeCode,'_basic .stat_total td:nth-child(11), ',
                        homeCode,'_basic .stat_total td:nth-child(12), ',homeCode,'_basic .stat_total td:nth-child(13), ',
                        homeCode,'_basic .stat_total td:nth-child(14), ',homeCode,'_basic .stat_total td:nth-child(15), ',
                        homeCode,'_basic .stat_total td:nth-child(16), ',homeCode,'_basic .stat_total td:nth-child(17), ',
                        homeCode,'_basic .stat_total td:nth-child(18), ',homeCode,'_basic .stat_total td:nth-child(19), ',
                        homeCode,'_basic .stat_total td:nth-child(20), ',homeCode,"_basic .stat_total td:nth-child(21)")) %>%
      html_text()
    
    ## Replace missing with 0s
    
    homeTeamStatsAdv <- replace(homeTeamStatsAdv, homeTeamStatsAdv == "", "0")
    
    ## Merge in team code with stats, then add date and opponent
    
    homeTeamStatsAdvFull <- data.frame(matrix(homeTeamStatsAdv, nrow=1))
    colnames(homeTeamStatsAdvFull) <- cols[2:length(cols)]
    homeTeamStatsAdvFull$team <- homeCode
    homeTeamStatsAdvFull$opponent <- awayCode
    homeTeamStatsAdvFull$date <- yesterday
    
    ###############  AWAY PLAYER STATS ##############
    
    ## Used to pull data from webpage - needs to be in this format for the list of players
    
    text <- paste0(awayCode,"_basic tbody td:nth-child(1)")
    
    awayPlayers <- boxUrlList2[[i]] %>%
      html_nodes(text) %>%
      html_text()
    
    ## Used to pull away player stats
    
    awayPlayerStats <- boxUrlList2[[i]] %>%
      html_nodes(paste0(awayCode,'_basic tbody td:nth-child(2), ',awayCode,'_basic tbody td:nth-child(3), ',
                        awayCode,'_basic tbody td:nth-child(4), ',awayCode,'_basic tbody td:nth-child(5), ',
                        awayCode,'_basic tbody td:nth-child(6), ',awayCode,'_basic tbody td:nth-child(7), ',
                        awayCode,'_basic tbody td:nth-child(8), ',awayCode,'_basic tbody td:nth-child(9), ',
                        awayCode,'_basic tbody td:nth-child(10), ',awayCode,'_basic tbody td:nth-child(11), ',
                        awayCode,'_basic tbody td:nth-child(12), ',awayCode,'_basic tbody td:nth-child(13), ',
                        awayCode,'_basic tbody td:nth-child(14), ',awayCode,'_basic tbody td:nth-child(15), ',
                        awayCode,'_basic tbody td:nth-child(16), ',awayCode,'_basic tbody td:nth-child(17), ',
                        awayCode,'_basic tbody td:nth-child(18), ',awayCode,'_basic tbody td:nth-child(19), ',
                        awayCode,'_basic tbody td:nth-child(20), ',awayCode,"_basic tbody td:nth-child(21)")) %>%
      html_text()
    
    ## Remove missings with 0, and drop "did not play" text 
    ## Then create a dataframe for all possible data - note players that did not play must be addressed next
    
    awayPlayerStats <- replace(awayPlayerStats, awayPlayerStats == "", "0")
    awayPlayerStats <- awayPlayerStats[!awayPlayerStats %in% "Did Not Play"]
    awayPlayerStats <- data.frame(matrix(awayPlayerStats, 
                                         ncol = 20, 
                                         nrow = (length(awayPlayerStats) / 20), 
                                         byrow = T),
                                  stringsAsFactors = F)
    
    
    ## IF there are players who didn't play, add zeros (using zeroRow) for every stat
    ## Necessary because basketball-reference has a cross-column text field saying "did not play"
    
    if(length(awayPlayers) > nrow(awayPlayerStats)) {
      for (j in (nrow(awayPlayerStats)+1): length(awayPlayers)) {
        awayPlayerStats <- rbind(awayPlayerStats, zeroRow)
      }
    } 
    
    ## Merge player information with their statistics, then add in team code & date of the game
    
    awayPlayerStatsFull <- data.frame(awayPlayers, awayPlayerStats)
    colnames(awayPlayerStatsFull) <- cols
    awayPlayerStatsFull$team <- awayCode
    awayPlayerStatsFull$opponent <- homeCode
    awayPlayerStatsFull$date <- yesterday
    
    ################ AWAY ADVANCED PLAYER STATS ####################
    
  
    ## Replicate exact same process for advanced stats
    ## Just a matter of changing selections
    
    awayPlayerStatsAdv <- boxUrlList2[[i]] %>%
      html_nodes(paste0(awayCode,'_advanced tbody td:nth-child(2), ',awayCode,'_advanced tbody td:nth-child(3), ',
                        awayCode,'_advanced tbody td:nth-child(4), ',awayCode,'_advanced tbody td:nth-child(5), ',
                        awayCode,'_advanced tbody td:nth-child(6), ',awayCode,'_advanced tbody td:nth-child(7), ',
                        awayCode,'_advanced tbody td:nth-child(8), ',awayCode,'_advanced tbody td:nth-child(9), ',
                        awayCode,'_advanced tbody td:nth-child(10), ',awayCode,'_advanced tbody td:nth-child(11), ',
                        awayCode,'_advanced tbody td:nth-child(12), ',awayCode,'_advanced tbody td:nth-child(13), ',
                        awayCode,'_advanced tbody td:nth-child(14), ',awayCode,'_advanced tbody td:nth-child(15), ',
                        awayCode,'_advanced tbody td:nth-child(16)')) %>%
      html_text()
    
    ## Remove missings with 0, and drop "did not play" text 
    ## Then create a dataframe for all possible data - note players that did not play must be addressed next
    
    awayPlayerStatsAdv <- replace(awayPlayerStatsAdv, awayPlayerStatsAdv == "", "0")
    awayPlayerStatsAdv <- awayPlayerStatsAdv[!awayPlayerStatsAdv %in% "Did Not Play"]
    awayPlayerStatsAdv <- data.frame(matrix(awayPlayerStatsAdv, 
                                         ncol = 15, 
                                         nrow = (length(awayPlayerStatsAdv) / 15),  ## Note the change here
                                         byrow = T),
                                  stringsAsFactors = F)
    
    
    ## IF there are players who didn't play, add zeros (using zeroRow) for every stat
    ## Necessary because basketball-reference has a cross-column text field saying "did not play"
    
    if(length(awayPlayers) > nrow(awayPlayerStatsAdv)) {
      for (j in (nrow(awayPlayerStatsAdv)+1): length(awayPlayers)) {
        awayPlayerStatsAdv <- rbind(awayPlayerStatsAdv, zeroRow)
      }
    } 
    
    ## Merge player information with their statistics, then add in team code & date of the game
    
    awayPlayerStatsAdvFull <- data.frame(awayPlayers, awayPlayerStatsAdv)
    colnames(awayPlayerStatsAdvFull) <- colsAdv
    awayPlayerStatsAdvFull$team <- awayCode
    awayPlayerStatsAdvFull$opponent <- homeCode
    awayPlayerStatsAdvFull$date <- yesterday
    
    ############# AWAY TEAM STATS ####################
    
    awayTeamStats <- boxUrlList2[[i]] %>%
      html_nodes(paste0(awayCode,'_basic .stat_total td:nth-child(2), ',awayCode,'_basic .stat_total td:nth-child(3), ',
                        awayCode,'_basic .stat_total td:nth-child(4), ',awayCode,'_basic .stat_total td:nth-child(5), ',
                        awayCode,'_basic .stat_total td:nth-child(6), ',awayCode,'_basic .stat_total td:nth-child(7), ',
                        awayCode,'_basic .stat_total td:nth-child(8), ',awayCode,'_basic .stat_total td:nth-child(9), ',
                        awayCode,'_basic .stat_total td:nth-child(10), ',awayCode,'_basic .stat_total td:nth-child(11), ',
                        awayCode,'_basic .stat_total td:nth-child(12), ',awayCode,'_basic .stat_total td:nth-child(13), ',
                        awayCode,'_basic .stat_total td:nth-child(14), ',awayCode,'_basic .stat_total td:nth-child(15), ',
                        awayCode,'_basic .stat_total td:nth-child(16), ',awayCode,'_basic .stat_total td:nth-child(17), ',
                        awayCode,'_basic .stat_total td:nth-child(18), ',awayCode,'_basic .stat_total td:nth-child(19), ',
                        awayCode,'_basic .stat_total td:nth-child(20), ',awayCode,"_basic .stat_total td:nth-child(21)")) %>%
      html_text()
    
    ## Replace missing with 0s
    
    awayTeamStats <- replace(awayTeamStats, awayTeamStats == "", "0")
    
    ## Merge in team code with stats, then add date and opponent
    
    awayTeamStatsFull <- data.frame(matrix(awayTeamStats, nrow=1))
    colnames(awayTeamStatsFull) <- cols[2:length(cols)]
    awayTeamStatsFull$team <- awayCode
    awayTeamStatsFull$opponent <- homeCode
    awayTeamStatsFull$date <- yesterday
    
    ############# AWAY TEAM STATS ####################
    
    awayTeamStatsAdv <- boxUrlList2[[i]] %>%
      html_nodes(paste0(awayCode,'_basic .stat_total td:nth-child(2), ',awayCode,'_basic .stat_total td:nth-child(3), ',
                        awayCode,'_basic .stat_total td:nth-child(4), ',awayCode,'_basic .stat_total td:nth-child(5), ',
                        awayCode,'_basic .stat_total td:nth-child(6), ',awayCode,'_basic .stat_total td:nth-child(7), ',
                        awayCode,'_basic .stat_total td:nth-child(8), ',awayCode,'_basic .stat_total td:nth-child(9), ',
                        awayCode,'_basic .stat_total td:nth-child(10), ',awayCode,'_basic .stat_total td:nth-child(11), ',
                        awayCode,'_basic .stat_total td:nth-child(12), ',awayCode,'_basic .stat_total td:nth-child(13), ',
                        awayCode,'_basic .stat_total td:nth-child(14), ',awayCode,'_basic .stat_total td:nth-child(15), ',
                        awayCode,'_basic .stat_total td:nth-child(16), ',awayCode,'_basic .stat_total td:nth-child(17), ',
                        awayCode,'_basic .stat_total td:nth-child(18), ',awayCode,'_basic .stat_total td:nth-child(19), ',
                        awayCode,'_basic .stat_total td:nth-child(20), ',awayCode,"_basic .stat_total td:nth-child(21)")) %>%
      html_text()
    
    ## Replace missing with 0s
    
    awayTeamStatsAdv <- replace(awayTeamStatsAdv, awayTeamStatsAdv == "", "0")
    
    ## Merge in team code with stats, then add date and opponent
    
    awayTeamStatsAdvFull <- data.frame(matrix(awayTeamStatsAdv, nrow=1))
    colnames(awayTeamStatsAdvFull) <- cols[2:length(cols)]
    awayTeamStatsAdvFull$team <- awayCode
    awayTeamStatsAdvFull$opponent <- homeCode
    awayTeamStatsAdvFull$date <- yesterday
    
    ## Put home & away team & player stats into a list
    
    homeStats[[i]] <- homePlayerStatsFull
    homeStatsAdv[[i]] <- homePlayerStatsAdvFull
    homeStatsTeam[[i]] <- homeTeamStatsFull
    homeStatsTeamAdv[[i]] <- homeTeamStatsAdvFull
    awayStats[[i]] <- awayPlayerStatsFull
    awayStatsAdv[[i]] <- awayPlayerStatsAdvFull
    awayStatsTeam[[i]] <- awayTeamStatsFull
    awayStatsTeamAdv[[i]] <- awayTeamStatsAdvFull
      
  }
}

#### Convert the lists of dataframes into stacked dataframe, then save as csv

unlistDF <- function(x) {
  do.call(rbind.data.frame, x)
}

homeStatsDF <- unlistDF(homeStats)
homeStatsAdvDF <- unlistDF(homeStatsAdv)
homeStatsTeamDF <- unlistDF(homeStatsTeam)
homeStatsTeamAdvDF <- unlistDF(homeStatsTeamAdv)
awayStatsDF <- unlistDF(awayStats)
awayStatsAdvDF <- unlistDF(awayStatsAdv)
awayStatsTeamDF <- unlistDF(awayStatsTeam)
awayStatsTeamAdvDF <- unlistDF(awayStatsTeamAdv)

playerStatsFin <- rbind(homeStatsDF, awayStatsDF)
playerStatsAdvFin <- rbind(homeStatsAdvDF, awayStatsAdvDF)
teamStatsFin <- rbind(homeStatsTeamDF, awayStatsTeamDF)
teamStatsAdvFin <- rbind(homeStatsTeamAdvDF, awayStatsTeamAdvDF)

## TO DO

## Replace 0s in team +/- with actual outcome
## Create binary outcome variable for win/loss in team stats
## Pull historic data
## Figure out way to automate process and store elsewhere
## Merge basic & advanced stats
## Move team name to front in team stats
## Convert data into proper format (numeric)

setwd('~/Documents/Northwestern/498/NBA Scraped')

## Save to folder in on C Drive

csvSaver <- function(x, type) {
  write.csv(x, paste0(type, "_", yesterday, '.csv'), row.names = F)
}

csvSaver(playerStatsFin, 'player')
csvSaver(playerStatsAdvFin, 'playerAdv')
csvSaver(teamStatsFin, 'team')
csvSaver(teamStatsAdvFin, 'teamAdv')
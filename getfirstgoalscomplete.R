#####Written by Henry Shipper

library(plyr) ###for ldply()
library(dplyr) ###don't think it's necessary but doesn't hurt
library(XML) ###for readHTMLTable()
library(RCurl) ###for getURI()
library(lattice) ###for plots

fetch_players <- function(letter) { ###gets literally all player hashes in database
  url <- paste0("http://www.hockey-reference.com/players/", letter, "/skaters.html")
  txt <- getURI(url) ###gets source code of site
  matches <- gregexpr(paste0("/players/", letter, "/[a-z]{5,7}[0-9]{2}.html"), txt) ###finds /players/abcdefg01.html
  ids <- regmatches(txt, matches) ###shows all matches
  ids <- as.data.frame(ids) ###dataframe cooperates with ldply, allows sorting by letter
  ids$letter <- letter ###adds column to sort by letter
  ids ### returns data.frame with ids, letter
  #####this function could be cleaned up to produce a vector of ids and vector of letters rather than df
  #####df has lots of NA's
}

#####this section is to aggregate the ids into a usable atomic vector
test <- ldply(letters[-24], fetch_players, .progress = "text") #####no last names start with "x"
newt <- test[test$letter == "a", 1] ###create vector with just id's starting with a
j = 3 ###for iteration
newt <- as.vector(newt) ###changes from dataframe to vector? might be unneccessary

for(i in letters[c(-1,-24)]){ ###ids starting with a already in vector, none start with x
  newt <- c(newt, as.vector(test[test$letter == i, j]))
  j <- j+1
}

#####this section passes players who entered the league after expansion, scored a goal, and the year they scored
check_player <- function(player_id){
  player_id <- gsub("(.*)(.html)", "\\1", player_id) ###allows format from vector to be usable
  url <- paste0("http://www.hockey-reference.com", player_id, ".html")
  db <- readHTMLTable(url, stringsAsFactors = FALSE)
  reg <- db$stats_basic_nhl ###for easier calls to regular season stats
  reg$Season <- gsub("(.{2})(.{3})(.{2})", "\\1\\3", reg$Season) ###changes 2008-09 to 2009. needed for getting gamelogs
  reg$Season <- as.numeric(reg$Season)
  if(reg$Season[1] >= 2001){ ###removes players who entered before expansion
    reg$G <- as.numeric(reg$G)
    goals <- which(reg$G >= 1)
    if(length(goals) != 0){ ###removes players who didn't score any goals
      yr <- reg$Season[min(goals)]
      c(player_id, yr) ###passes player_id and yr they scored a goal
    }
  }
}

scorers <- ldply(newt, check_player, .progress = "text")
scorers.list <- split(scorers, rownames(scorers))
names(scorers.list) <- c("player_id", "yr")

#####this section finds the game that player scored their first goal in and adds it to a dataframe
grab_games <- function(row){
  player_id <- row$player_id
  yr <- row$yr
  url <- paste0("http://www.hockey-reference.com", player_id, "/gamelog/", yr, "/")
  games <- readHTMLTable(url, stringsAsFactors = FALSE)
  reg <- games$stats[, -1]
  names(reg) <- c("Games", "Age", "Date", "Team", "Road", "Opp", "Result", "Goals", "Assists", "Points", "Plus-Minus", "PIM", "EVG", "PPG", "SHG", "GWG", "EVA", "PPA", "SHA", "Shots", "Shot%", "Shifts", "TOI")
  reg$Goals <- as.numeric(reg$Goals)
  reg <- reg[!is.na(reg$Goals), ]
  reg[min(which(reg$Goals >= 1)), ]
}

first_goals <- ldply(scorers.list, grab_games, .progress = "text")
first <- cbind(praying$player_id, first_goals)
first$Team <- gsub("ATL", "WPG", first$Team)
first$Team <- gsub("PHX", "ARI", first$Team)
first$Team <- gsub("MDA", "ANA", first$Team)
first$Team <- as.factor(first$Team)
first$Opp <- gsub("ATL", "WPG", first$Opp)
first$Opp <- gsub("PHX", "ARI", first$Opp)
first$Opp <- gsub("MDA", "ANA", first$Opp)
first$Opp <- as.factor(first$Opp)



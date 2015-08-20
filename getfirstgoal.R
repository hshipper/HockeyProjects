getfirstscoringseason <- function(id){
  first_goal_list <- list()
  seasons_url <- paste0("http://www.hockey-reference.com", id)
  seasons_list <- readHTMLTable(seasons_url, stringsAsFactors = FALSE)
  seasons_df <- determine_df(seasons_list)
  names(seasons_df)[15:17] <- c("EVA", "PPA", "SHA")
  goal_seasons <-
    seasons_df %>%
      mutate(Season = as.numeric(gsub("(.{5})(.{2})", "20\\2", Season))) %>%
      mutate(G = as.numeric(G)) %>%
      filter(G >= 1)
  first_season <- goal_seasons[[1,1]]
}

getfirstgoalgame <- function(id){
  first_season <- getfirstscoringseason(id)
  first_goal_list <- list()
  log_id <- gsub("(.*)(.html)", "\\1", id)
  log_url <- paste0("http://www.hockey-reference.com", log_id, "/gamelog/", first_season)
  gamelog <- readHTMLTable(log_url, stringsAsFactors = FALSE)$gamelog
  names(gamelog)[c(2, 6, 8, 18:20)] <- c("Games", "At", "W-L", "EVA", "PPA", "SHA")
  goal_games <-
    gamelog %>%
      mutate(G = as.numeric(G)) %>%
      filter(G >= 1)
  first_goal_game <- goal_games[1,]
  first_goal_game[["id"]] <- id
  first_goal_list[[id]] <- first_goal_game
  first_goal_list[[id]]
}

determine_df <- function(list){
  if (names(list)[1] != "NULL") list[[1]] else list[[2]]
}
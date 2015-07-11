check_player <- function(player_id){
  player_id <- gsub("(.*)(.html)", "\\1", player_id)
  url <- paste0("http://www.hockey-reference.com", player_id, ".html")
  db <- readHTMLTable(url, stringsAsFactors = FALSE)
  reg <- db$stats_basic_nhl 
  reg$Season <- gsub("(.{2})(.{3})(.{2})", "\\1\\3", reg$Season)
  reg$Season <- as.numeric(reg$Season)
  if(reg$Season[1] >= 2001){
    reg$G <- as.numeric(reg$G)
    goals <- which(reg$G >= 1)
      if(length(goals) != 0){
      yr <- reg$Season[min(goals)]
      c(player_id, yr)
      }
  }
}
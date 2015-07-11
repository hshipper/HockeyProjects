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
fetch_players <- function(letter){
  player_tables <- list()
  url <- paste0("http://www.hockey-reference.com/players/", letter, "/skaters.html")
  text <- getURI(url)
  player_tables[[letter]] <- readHTMLTable(text)$skaters
  matches <- gregexpr(paste0("/players/", "[a-z]/[a-z]{4,7}[0-9]{2}.html"), text)
  ids <- regmatches(text, matches)
  player_tables[[letter]][["id"]] <- ids[[1]]
  player_tables[[letter]]
}

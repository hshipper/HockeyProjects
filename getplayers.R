fetch_players <- function(letter) {
  url <- paste0("http://www.hockey-reference.com/players/", letter, "/skaters.html")
  txt <- getURI(url)
  matches <- gregexpr(paste0("/players/", letter, "/[a-z]{7}[0-9]{2}.html"), txt)
  ids <- regmatches(txt, matches)
  ids <- as.data.frame(ids)
  ids$letter <- letter
  ids
}


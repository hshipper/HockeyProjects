fetch_players2 <- function(letter) {
  url <- paste0("http://www.hockey-reference.com/players/", letter, "/skaters.html")
  txt <- getURI(url)
  matches <- gregexpr(paste0('/players/', letter, '/[a-z]{5,7}[0-9]{2}.html">([[:alpha:]])*([[:space:]])([[:alpha:]])*</a>'), txt)
  ids <- regmatches(txt, matches)[[1]]
  urlSuffix <- gsub('(/players/[[:alpha:]]/[[:alpha:]]{5,7}[[:digit:]]{2}.html)\\">([A-Z][a-z]*.[A-Z][a-z]*)</a>',
               '\\1', ids)
  Player <- gsub('(/players/[[:alpha:]]/[[:alpha:]]{5,7}[[:digit:]]{2}.html)\\">([A-Z][a-z]*.[A-Z][a-z]*)</a>',
                '\\2', ids)
  pairs <- cbind(urlSuffix, Player)
  DF <- (readHTMLTable(url, stringsAsFactors = FALSE)$skaters[c(1,2,6)])
  singleDF <- merge(DF, pairs, by.y = "Player")
  singleDF <- singleDF[singleDF$G > 0 & singleDF$From >= 2001, ]
  singleDF
}

get_year <- function(urlSuffix){
  url <- paste0("http://www.hockey-reference.com", urlSuffix)
  db <- readHTMLTable(url, stringsAsFactors = FALSE)
  reg <- db$stats_basic_nhl ###for easier calls to regular season stats
  reg$Season <- gsub("(.{2})(.{3})(.{2})", "\\1\\3", reg$Season) ###changes 2008-09 to 2009. needed for getting gamelogs
  reg$Season <- as.numeric(reg$Season)
  reg$G <- as.numeric(reg$G)
  goals <- which(reg$G >= 1)
  year <- reg$Season[min(goals)]
  c(urlSuffix, year)
}








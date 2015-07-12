fetch_players2 <- function(letter) {
  url <- paste0("http://www.hockey-reference.com/players/", letter, "/skaters.html")
  txt <- getURI(url)
  matches <- gregexpr(paste0('/players/', letter, '/[a-z]{5,7}[0-9]{2}.html">([[:alpha:]])*([[:space:]])([[:alpha:]])*</a>'), txt)
  ids <- regmatches(txt, matches)[[1]]
  urlSuffix <- gsub('(/players/[[:alpha:]]/[[:alpha:]]{5,7}[[:digit:]]{2}.html)\\">([A-Z][a-z]*.[A-Z][a-z]*)</a>',
               '\\1', ids)
  Player <- gsub('(/players/[[:alpha:]]/[[:alpha:]]{5,7}[[:digit:]]{2}.html)\\">([A-Z][a-z]*.[A-Z][a-z]*)</a>',
                '\\2', ids)
  pairs <- data.table(urlSuffix, Player)
  DT <- data.table(readHTMLTable(url, stringsAsFactors = FALSE)$skaters[c(1,2,6)])
  singleDT <- merge(DT, pairs, by = "Player")
  singleDT <- completeDT[G > 0 & From >= 2001]
  singleDT
}





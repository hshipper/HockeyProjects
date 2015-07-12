#####High level commands for updated code: foo_bar2()

compositeDF <- ldply(letters[-24], fetch_players2, .progress = "text")[,c(1,4)]
yearDF <- ldply(compositeDF$urlSuffix, get_year, .progress = "text")

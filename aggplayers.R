test <- ldply(letters[-24], fetch_players, .progress = "text")

newt <- test[test$letter == "a", 1]
j = 3
newt <- as.vector(newt)

for(i in letters[c(-1,-24)]){
  newt <- c(newt, as.vector(test[test$letter == i, j]))
  j <- j+1
}

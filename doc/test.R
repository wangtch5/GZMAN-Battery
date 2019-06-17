setwd("C:/Users/twang/Documents/GE/Data")
filenames <- list.files()[1:6]

dlist <- list()
for(i in 1:length(filenames)){
  dlist[[i]] <- read.csv(filenames[i])
}

test <- dlist[[1]]
test$LocalTime <- as.character(test$LocalTime)
test$Time <- sapply(test$LocalTime, function(x){
  strsplit(x, " ")[[1]][2]
})
test$Date <- sapply(test$LocalTime, function(x){
  strsplit(x, " ")[[1]][1]
})
test$Date <- as.Date(test$Date, format = "%m/%d/%y")

filenames
nameMw <- c(95, 35, 118, 54, 17, 27)
allMW <- sapply(dlist, function(x){
  sum(x$Power.MW.)
})
# nameMw/17
# allMW/allMW[5]
cor(nameMw, allMW)

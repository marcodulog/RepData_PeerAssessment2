require(sqldf)
setwd("c:/files/r")
ds <- read.csv(file=bzfile('StormData.csv.bz2'),strip.white = TRUE)
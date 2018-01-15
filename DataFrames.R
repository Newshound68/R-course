# This is our initial R script. 18.05.2015. Updated on 03.01.2018.
rm(list = ls())			              

options(digits = 14, width = 100)

dbh <- c(33.5, 40.5, 39.0, 54.0, 38.8, 32.4, 29.7, 31.0, 55.5, 26.1)
h <- c(28.9, 30.0, 26.6, 30.7, 27.0, 26.2, 27.8, 25.5, 31.7, 24.8)
myData <- data.frame(dbh, h)
str(myData)
myData$h[2]

mean(myData$dbh)
with(myData, mean(dbh)) # New

(myData.s <- myData[order(myData$dbh, decreasing = TRUE), ])

myData.s[1 : 5, ]
head(myData.s) # new
tail(myData.s) # new

myData.s[2]

myData.s["h"]

sapply(myData, mean)

apply(myData, 1, mean) # row means
apply(myData, 2, mean) # column means
?apply
myData$hd <- 100 * myData$h / myData$dbh
myData[1 : 3, ]

myData[myData$dbh > 35, ]

myData[myData$dbh > 30 & myData$hd > 80, ]

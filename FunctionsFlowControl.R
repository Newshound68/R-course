# This is our initial R script. 18.05.2015. Updated on 30.03.2017.
rm(list = ls())			              

options(digits = 14, width = 100)

# xfilePath <- "Z:/Dysgu/ResearchSchool/Modules/AnalysisOfVariance/InitialR/"
xfilePath <- "/Users/arng0001/Dropbox/AnalysisOfVariance/InitialR/"
myData <- read.table(paste(xfilePath, "Data1.txt", sep = ""), header = TRUE)
# Functions
calcQuadraticMean <- function(myVector) {						
  x <- sqrt(sum(myVector^2) / length(myVector))
  return(x)
}

calcQuadraticMean(myData$dbh)

tapply(myData$dbh, myData$species, sum)
# aggregate(myData, myData$species, calcQuadraticMean)
# ?aggregate
mean(myData$dbh)

mean.xfor <- 0
for (i in 1 : length(myData$dbh)) {
  mean.xfor <- mean.xfor + myData$dbh[i]
}
mean.xfor / length(myData$dbh)
# mean(myData$dbh)

mean.xwhile <- 0
i <- 1
while (i < length(myData$dbh) + 1) {
  mean.xwhile <- mean.xwhile + myData$dbh[i]
  i <- i + 1
}
mean.xwhile <- mean.xwhile / (i - 1)
mean.xwhile

mean.xrepeat <- 0
i <- 1
repeat {
  mean.xrepeat <- mean.xrepeat + myData$dbh[i]
  i <- i + 1
  if(i == length(myData$dbh) + 1)
    break
}
mean.xrepeat <- mean.xrepeat / (i - 1)
mean.xrepeat

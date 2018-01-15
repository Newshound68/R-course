rm(list = ls())	
# xfilePath <- "Z:/Dysgu/ResearchSchool/Modules/AnalysisOfVariance/InitialR/"
xfilePath <- "/Users/arng0001/Dropbox/AnalysisOfVariance/InitialR/"
myData <- read.table(paste(xfilePath, "Data1.txt", sep = ""), header = TRUE)

dim(myData)
names(myData)

mean(myData$dbh, na.rm = T)
myData <- myData[!is.na(myData$dbh), ]

write.table(myData, file = paste(xfilePath, "Data1new.txt", sep = ""))
write.csv(myData, file = paste(xfilePath, "Data1new.csv", sep = ""))

myData.LP1 <- myData[myData$species == 3, ]
length(myData.LP1$treeno)
myData.LP2 <- subset(myData, species == 12)
length(myData.LP2$treeno)

tapply(myData$treeno, myData$species, length)
table(myData$species)
by(myData$species, myData$species, length)

range(myData$dbh)

hist(myData$dbh)

# New
xfile <- "/Users/arng0001/Dropbox/Gwaith/Rcourse/"
source(paste(xfile, "ExternalExample.R", sep = ""), echo = TRUE) # Processing external code


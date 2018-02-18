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

# New: data merging
rm(list = ls())

# Plot 1, first time step (1988 - 1992)
plot1.1988 <- read.csv("/Users/arng0001/Dropbox/Gwaith/Rcourse/plot1t_1988.csv",header=T)
names(plot1.1988)
plot1.1988 <- plot1.1988[order(plot1.1988$Treeno, decreasing = FALSE), ]
plot1.1988$Plotno <- c(1)
plot1.1988$year <- c(1988)
plot1.1988 <- plot1.1988[plot1.1988$Treeno > 0, ]
plot1.1988 <- plot1.1988[plot1.1988$status != "dead" & plot1.1988$status != "cut", ]

plot1.1992 <- read.csv("/Users/arng0001/Dropbox/Gwaith/Rcourse/plot1t_1992.csv",header=T)
names(plot1.1992)
plot1.1992 <- plot1.1992[order(plot1.1992$Treeno, decreasing = FALSE), ]
plot1.1992$Plotno <- c(1)
plot1.1992$year <- c(1992)
plot1.1992 <- plot1.1992[plot1.1992$Treeno > 0, ]
plot1.1992 <- plot1.1992[plot1.1992$status != "dead" & plot1.1992$status != "cut", ]

plot1.1992$dbh1992 <- plot1.1992$dbh
plot1.1992s <- plot1.1992[c("Treeno","dbh1992")]
plot1.1988 <- merge(plot1.1988, plot1.1992s, all.x = TRUE, sort = TRUE)
plot1.1988$AGR <- (plot1.1988$dbh1992 - plot1.1988$dbh) / (1992 - 1988)
plot1.1988$RGR <- (log(plot1.1988$dbh1992) - log(plot1.1988$dbh)) / (1992 - 1988)

growthRates <- plot1.1988[c("Plotno", "Treeno", "Species", "x" ,"y", "dbh", "dbh1992", "AGR", "RGR", "year")] 
head(growthRates)

rm(plot1.1988)
rm(plot1.1992)
rm(plot1.1992s)

# xfilePath <- "/Users/arng0001/Dropbox/Gwaith/Cyhoeddi/rScatterPlot/"
# pdf(file = paste(xfilePath, "rScatterPlotManderscheid.pdf", sep = ""))
par(mar = c(2, 3.2, 0.5, 0.5))
plot(growthRates$dbh, growthRates$AGR, pch = 16, xlab = "", ylab = "", axes = FALSE, xlim = c(0, 90), ylim = c(0, 0.6))	
axis(1, lwd = 2, cex.axis = 1.8)
axis(2, las = 1, lwd = 2, cex.axis = 1.8)
box(lwd = 2)
# dev.off()

# xfilePath <- "/Users/arng0001/Dropbox/Gwaith/Cyhoeddi/rScatterPlot/"
# pdf(file = paste(xfilePath, "rScatterPlotManderscheid.pdf", sep = ""))
par(mar = c(2, 4.2, 0.5, 0.5))
plot(growthRates$dbh, growthRates$RGR, pch = 16, xlab = "", ylab = "", axes = FALSE, xlim = c(0, 90), ylim = c(0, 0.3))	
axis(1, lwd = 2, cex.axis = 1.8)
axis(2, las = 1, lwd = 2, cex.axis = 1.8)
box(lwd = 2)
# dev.off()

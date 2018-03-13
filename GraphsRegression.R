# This is our initial R script. 18.05.2015. Updated on 30.03.2017.
rm(list = ls())			              
options(digits = 14, width = 100)

xfilePath <- "/Users/arng0001/Dropbox/Gwaith/Rcourse/"
# setwd("/Users/arng0001/Dropbox/Gwaith/Rcourse")

myData <- read.table(paste(xfilePath, "Data1.txt", sep = ""), header = TRUE)
View(myData) # Gives a data table

# png(file = paste(xfilePath, "histExample.png", sep = ""))
hist1 <- hist(myData$dbh, breaks = seq(6, 64, by = 4), include.lowest = T, right = F, plot = FALSE)
hist1$counts <- hist1$counts / length(myData$dbh)
par(mar = c(2, 4, 1, 2))
plot(hist1, las = 1, cex.axis = 1.7, lwd = 2, ylim = c(0, 0.25), main = "", xlab = "", ylab = "") # DBH [cm], Rel. frequency
plot(hist1, las = 1, cex.axis = 1.7, lwd = 2, ylim = c(0, 0.25), main = "", xlab = "", ylab = "", density = 15,  add = TRUE, axes = FALSE, col = "white")
box(lwd = 2)
# dev.off()

?hist

myData$dbhcls <- 4 * (round((myData$dbh + 0.00000001) / 4) - 1) + 4
dbhDistribution <- tapply(myData$dbh, myData$dbhcls, length)
sum(dbhDistribution)
length(dbhDistribution)
xdbhDistribution <- data.frame(cbind(dbhDistribution))
xdbhDistribution$numbers <- xdbhDistribution$dbhDistribution
xdbhDistribution$dbhcls <- as.numeric(row.names(xdbhDistribution)) 
xdbhDistribution <- xdbhDistribution[c("dbhcls", "numbers")]
tdbhDistribution <- xdbhDistribution$numbers / sum(dbhDistribution) 
names <- xdbhDistribution$dbhcls
# pdf(file = paste(xfilePath, "barExample.pdf", sep = ""))
par(mar = c(3, 4, 3, 4))
barplot(tdbhDistribution, beside = TRUE, ylab = "",  main = "", col = rgb(0, 0, 0, 0), las = 1,  axes = TRUE, names.arg = names, cex.names = 1.7, cex.axis = 1.7, lwd = 2,) 
barplot(tdbhDistribution, beside = TRUE, ylab = "", main = "", lwd = 1, col = rgb(0, 0, 0, 0), density = 10,  add = TRUE, axes = FALSE) 
# dev.off()	

# pdf(file = paste(xfilePath, "scatterExample.pdf", sep = ""))
par(mar = c(2, 3, 0.5, 0.5))
plot(myData$dbh, myData$h, xlim = c(0, 65), ylim = c(0, 35), pch = 16, xlab = "", ylab = "", axes = FALSE)	
axis(1, lwd = 2, cex.axis = 1.8)
axis(2, las = 1, lwd = 2, cex.axis = 1.8)
box(lwd = 2)
nlm.Petterson <- nls(h ~ 1.3 + (dbh/(a + b * dbh))^3, data = myData, start = list(a = 0.5, b = 3.8), trace = T)
summary(nlm.Petterson)												
e.a <- summary(nlm.Petterson)$coefficients[1]
curve(1.3 + (x/(summary(nlm.Petterson)$coefficients[1] + summary(nlm.Petterson)$coefficients[2] * x))^3, from = min(myData$dbh),  to = max(myData$dbh), lwd = 1, lty = 1, col = "red", add = TRUE) 
# dev.off()
?pch

# pdf(file = paste(xfilePath, "scatterExample.pdf", sep = ""))
par(mar = c(2, 3, 0.5, 0.5))
plot(myData$dbh, myData$h, xlim = c(0, 65), ylim = c(0, 35), pch = 16, xlab = "", ylab = "", axes = FALSE)	
axis(1, lwd = 2, cex.axis = 1.8)
axis(2, las = 1, lwd = 2, cex.axis = 1.8)

loss.L2 <- function(abdn, xdata) {
  yh <- xh(abdn[1 : 2], xdata)
  xdev <- xdata$h - yh
  return(sum(xdev^2, na.rm = TRUE))
}

# Petterson
xh <- function(abdn, xdata) {
  yh <- 1.3 + (xdata$dbh/(abdn[1] + abdn[2] * xdata$dbh))^3
  return(yh)	
}

abdn0 <- c(0.5, 3.8)
abdn.L2 <- optim(abdn0, loss.L2, xdata = myData, control = list(maxit = 30000, temp = 2000, trace = TRUE, REPORT = 500))

abdn.L2$par

curve(1.3 + (x/(abdn.L2$par[1] + abdn.L2$par[2] * x))^3, from = min(myData$dbh),  to = max(myData$dbh), lwd = 1, lty = 1, col = "black", add = TRUE) 

axis(1, lwd = 2, cex.axis = 1.8)
axis(2, las = 1, lwd = 2, cex.axis = 1.8)
box(lwd = 2)
# dev.off()


rm(list = ls())																
# xfilePath <- "Z:/Dysgu/ResearchSchool/Modules/AnalysisOfVariance/InitialR/"
xfilePath <- "/Users/arng0001/Dropbox/AnalysisOfVariance/InitialR/"
xfile <- read.table(paste(xfilePath, "Bialowieza.out", sep = ""), header = TRUE)
# par(mfrow = c(1, 2))
# pdf(file = paste(xfilePath, "BialowiezaGa.pdf", sep = ""))
par(mar = c(4, 5, 3.5, 0.5))
plot(xfile$r, xfile$ga, xlab="Distance", ylab="Gamma", main = "Spatial Analysis", type = "l", las = 1, ylim = c(0,350), axes = FALSE, lwd = 2)		
lines(xfile$r, xfile$gamin, type = "l", col = "black", lty = 3, lwd = 2)				
lines(xfile$r, xfile$gamax, type = "l", col = "black", lty = 3, lwd = 2)	
axis(1, lwd = 2, cex.axis = 1.7)
axis(2, las = 1, lwd = 2, cex.axis = 1.7)
box(lwd = 2)
# dev.off()

# Specify parameters of Chapman-Richards growth function
A <- 35.204						
k <- 0.0235
p <- 1.237
# Draw the curve of the growth function
par(mar = c(4.5, 4.5, 2, 0.5))
curve(A * (1 - exp(-k * x))^p, from = 5, to = 120, xlab = "Age [years]", ylab = "Top height [m]", main = "SP (YC 14, ITh, 1-8m)", axes = FALSE, lwd = 2)
axis(1, lwd = 2, cex.axis = 0.8)
axis(2, las = 1, lwd = 2, cex.axis = 0.8)
box(lwd = 2)


rm(list = ls())	
# xfilePath <- "Z:/Dysgu/ResearchSchool/Modules/AnalysisOfVariance/InitialR/"
# xfilePath <- "/Users/arng0001/Dropbox/AnalysisOfVariance/InitialR/"
xfilePath <- "/Users/arng0001/Dropbox/Gwaith/Rcourse/"
myData <- read.table(paste(xfilePath, "Data1.txt", sep = ""), header = TRUE)
# pdf(file = paste(xfilePath, "BoxPlotExample.pdf", sep = ""))
my.labels <- c("LP", "SS")
# par(lab = c(length(my.labels), 5, 7), mar = c(2.5, 4, 0.5, 0.5))
par(mar = c(2.5, 4, 0.5, 0.5))
boxplot(dbh ~ species, data = myData, boxwex = 0.5, lwd = 2, axes = FALSE)
axis(1, at = 1 : 2, labels = my.labels, lwd = 2, cex.axis = 1.8)
axis(2, las = 1, lwd = 2, cex.axis = 1.8)
box(lwd = 2)
# dev.off()


rm(list = ls())			              
# xfilePath <- "Z:/Dysgu/ResearchSchool/Modules/AnalysisOfVariance/InitialR/"
xfilePath <- "/Users/arng0001/Dropbox/AnalysisOfVariance/InitialR/"
dbh <- c(42.4, 54.0, 32.2, 39.8, 26.1, 30.4, 39.9, 45.5, 37.9, 31.7)
id <- c(5.5, 7.2, 4.0, 5.1, 3.1, 3.8, 5.1, 6.0, 4.8, 4.0)

lm.inc <- lm(id ~ dbh)
summary(lm.inc)							

# pdf(file = paste(xfilePath, "qqnorm.pdf", sep = ""))
qqnorm(lm.inc$residuals)
qqline(lm.inc$residuals)
# dev.off()

(b <- cov(dbh, id) / var(dbh))
(a <- mean(id) - b * mean(dbh))
cor(dbh, id)^2

# pdf(file = paste(xfilePath, "linRegExample.pdf", sep = ""))
par(mar = c(2, 2, 0.5, 0.8))
plot(dbh, id, ylab = "", xlab = "", pch = 16, cex = 1.8, las = 1, cex.axis = 1.8, lwd = 2) 
lines(dbh, fitted(lm.inc), col = "black")
# abline(lm.inc, col = "black")
box(lwd = 2) 
# dev.off()

# Ele Census Graphs
## Meant to graph the ele numbers in different areas

# Set working directory
setwd("X:/nagelki4/Data/Census/GEC")

# read in the data
list.files()
GEC_data <- read.csv("GEC_trend_data_numbers_only_firstpage.csv", header = TRUE)

# Write a function for the park to get the ele numbers and the years and a bar plot
GEC_numbers <- function(data, ecosystem, stratum){
  x <- data[data$Ecosystem == ecosystem, ]
  x <- x[x$Stratum == stratum, ]
  y <- x[c("Year", "Estimate")]
  barplot(y$Estimate, names.arg = y$Year, main = stratum, ylim = c(0, 8000))
  y
}

# List the Ecosystems and Stratums just to help out
eco.list <- unique(sort(GEC_data$Ecosystem))
stratum.list <- unique(sort(GEC_data$Stratum))
matrix(eco.list)
matrix(stratum.list)

# Set up plotting window
par(mfrow=c(2,2))

# Run the function for the data you want
mara <- GEC_numbers(GEC_data, "Masai Mara", "Masai Mara R")
mara.unprotected <- GEC_numbers(GEC_data, "Masai Mara", "unprotected areas")
serengeti <- GEC_numbers(GEC_data, "Serengeti", "Serengeti")

# Do the total for the Mara and the surrounding areas
mara.tot <- mara
mara.tot$Estimate <- mara.tot$Estimate + mara.unprotected$Estimate

# Do total for Mara-Serengeti...the dates don't match, so just pick the ones that are clsoe
ms.tot <- mara[1,]

ms.tot <- matrix(ncol = 2, nrow = 4)
years <- c(1997, 2005, 2007, 2014)
colnames(ms.tot) <- c("Year", "Estimate")
ms.tot <- as.data.frame(ms.tot)
ms.tot$Year <- years
ms.tot$Estimate[1] <- mara.tot$Estimate[1] + serengeti$Estimate[1]
ms.tot$Estimate[2] <- mara.tot$Estimate[2] + serengeti$Estimate[3]
ms.tot$Estimate[3] <- mara.tot$Estimate[3] + serengeti$Estimate[4]
ms.tot$Estimate[4] <- mara.tot$Estimate[5] + serengeti$Estimate[5]


# Plot final plot
barplot(ms.tot$Estimate, names.arg = ms.tot$Year, main = "Mara-Serengeti Total", ylim = c(0, 8000))

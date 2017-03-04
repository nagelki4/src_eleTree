# This is written to create change maps and calc total area with TC change between years

# Load Libraries
library(raster)
library(rgdal)


# Set working directory
setwd("C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/Landsat_VCF")

file.list <- list.files()

mpala.2000 <- raster(file.list[1])
mpala.2005 <- raster(file.list[2])
mpala.2010 <- raster(file.list[3])
mpala.2015 <- raster(file.list[4])


# Folder with shape file of ground truth area
shape.folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/ParkData/Mpala/Boundary"
mpala.boundary.simple <- readOGR(shape.folder, "MpalaShapeforGoog")
# Project in same as rasters
prj1 <- crs(mpala.2000)
mpala.boundary.simple <- spTransform(mpala.boundary.simple, prj1)

# Crop down to mpala extent
mp.2000 <- crop(mpala.2000, extent(mpala.boundary.simple))
mp.2005 <- crop(mpala.2005, extent(mpala.boundary.simple))
mp.2010 <- crop(mpala.2010, extent(mpala.boundary.simple))
mp.2015 <- crop(mpala.2015, extent(mpala.boundary.simple))

# Clip to the Mpala poly
m.2000 <- mask(mp.2000, mpala.boundary.simple)
m.2005 <- mask(mp.2005, mpala.boundary.simple)
m.2010 <- mask(mp.2010, mpala.boundary.simple)
m.2015 <- mask(mp.2015, mpala.boundary.simple)

# VCF values of 200 and so on represent diff things like water, so set them to zero
m.2000[m.2000 > 100] <- 0
m.2005[m.2005 > 100] <- 0
m.2010[m.2010 > 100] <- 0
m.2015[m.2015 > 100] <- 0


# Change the margins back to the default
plot(m.2000, main = "2000")
plot(m.2005, main = "2005")
plot(m.2010, main = "2010")
plot(m.2015, main = "2015")

# Get the differences. Positive values will be increase in TC
m.2000.2005 <- m.2005 - m.2000
m.2005.2010 <- m.2010 - m.2005
m.2010.2015 <- m.2015 - m.2010
m.2000.2015 <- m.2015 - m.2000

# Get count of cells with a value
mpala.cell.count <- length(m.2000.2005[!is.na(m.2000.2005)])

# Plot
incr <- round(length(m.2000.2005[m.2000.2005 > 0])/mpala.cell.count, 2)
decr <- round(length(m.2000.2005[m.2000.2005 < 0])/mpala.cell.count, 2)
neut <- round(length(m.2000.2005[m.2000.2005 == 0])/mpala.cell.count, 2)
plot(m.2000.2005, main = paste0("VCF 2000 - 2005\n Increase: ", incr, " Decrease: ", decr, " No Change: ", neut))
incr <- round(length(m.2005.2010[m.2005.2010 > 0])/mpala.cell.count, 2)
decr <- round(length(m.2005.2010[m.2005.2010 < 0])/mpala.cell.count, 2)
neut <- round(length(m.2005.2010[m.2005.2010 == 0])/mpala.cell.count, 2)
plot(m.2005.2010, main = paste0("VCF 2005 - 2010\n Increase: ", incr, " Decrease: ", decr, " No Change: ", neut))
incr <- round(length(m.2010.2015[m.2010.2015 > 0])/mpala.cell.count, 2)
decr <- round(length(m.2010.2015[m.2010.2015 < 0])/mpala.cell.count, 2)
neut <- round(length(m.2010.2015[m.2010.2015 == 0])/mpala.cell.count, 2)
plot(m.2010.2015, main = paste0("VCF 2010 - 2015\n Increase: ", incr, " Decrease: ", decr, " No Change: ", neut))
incr <- round(length(m.2000.2015[m.2000.2015 > 0])/mpala.cell.count, 2)
decr <- round(length(m.2000.2015[m.2000.2015 < 0])/mpala.cell.count, 2)
neut <- round(length(m.2000.2015[m.2000.2015 == 0])/mpala.cell.count, 2)
plot(m.2000.2015, main = paste0("VCF 2000 - 2015\n Increase: ", incr, " Decrease: ", decr, " No Change: ", neut))

# Look at the averages
avg.2000 <- mean(m.2000[!is.na(m.2000)])
avg.2005 <- mean(m.2005[!is.na(m.2005)])
avg.2010 <- mean(m.2010[!is.na(m.2010)])
avg.2015 <- mean(m.2015[!is.na(m.2015)])

avg.list <- c(2000, avg.2000, 2005, avg.2005, 2010, avg.2010, 2015, avg.2015)
mtx <- matrix(avg.list, nrow = 4, ncol = 2, byrow = TRUE)

plot(mtx[, 1], mtx[, 2], type = "b", ylim = c(0,5), xlab = "Year", ylab = "Tree Cover (%)", main = "Mpala Average VCF Tree Cover: 2000-2015")

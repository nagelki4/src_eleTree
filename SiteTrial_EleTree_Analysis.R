# Test Code for Ele-Tree Analysis

# Testing approach using Mpala Research Centre


# Load packages
library(raster)
library(rgdal)
library(sp)
library(rgeos)

# Set working directory
setwd("C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/Mpala/Boundary")

# Find shapefile name
full.name <- list.files(pattern = ".shp")
length.name <- nchar(full.name)
shape.name <- substr(full.name, 1, length.name-4)


# Load shapefile
mpala.shape <- readOGR(dsn = ".", layer = shape.name) #readShapePoly("file.shp") is another option, but then have to define projection, I think
plot(mpala.shape)

# Report projection (might be needed later)
mpala.proj <- mpala.shape@proj4string # could also write this as crs(mpala.shape)


# Load table
MTE <- read.csv("C:/Users/nagelki-4/Dropbox/Permanent/Grad School/Projects/EleTree Analysis/Records/Main_Table_EleTree.csv", header = TRUE, row.names = 1)

# Calc area (km^2)
mpala.shape@polygons[[1]]@area
reported_area <- mpala.shape@data["REP_AREA"][[1]]
gis_area <- mpala.shape@data["GIS_AREA"][[1]]

# Find Lat Long
centroid <- getSpPPolygonsLabptSlots(mpala.shape)

# Plug in values to table
MTE["Mpala", "lat"] <- centroid[2]
MTE["Mpala", "long"] <- centroid[1]
MTE["Mpala", "site_area_km"] <- gis_area

# Write to table
write.csv(MTE, "C:/Users/nagelki-4/Dropbox/Permanent/Grad School/Projects/EleTree Analysis/Records/Main_Table_EleTree.csv", 
          row.names = TRUE)









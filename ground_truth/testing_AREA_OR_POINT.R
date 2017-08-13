setwd("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/ParkData/Kruger/ground_truth")


library(raster)
library(rgdal)
library(sp)


GEE <- raster("Kruger_backdrop_GEE.tif")
EE <- raster("Kruger_backdrop_USGSEarthExplorer_mosaic.tif")
delta <- raster("Kruger_change.tif")


GEE_sub <- rasterFromCells(GEE, c(1:8), values=TRUE)
EE_sub <- rasterFromCells(EE, c(1:8), values = TRUE)
d_sub <- rasterFromCells(delta, c(1:8), values = TRUE)
d_sub_shift <- shift(d_sub, 15, 15)
plot(EE_sub)

plot(d_sub_shift, add = TRUE)


d_sub@extent <- d_sub@extent+15
extent(d_sub_shift)
extent(EE_sub)
extent(d_sub)



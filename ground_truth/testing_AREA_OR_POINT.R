setwd("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/ParkData/Kruger/ground_truth")


library(raster)
library(rgdal)
library(sp)


GEE <- raster("Kruger_backdrop_GEE.tif")
EE <- raster("Kruger_backdrop_USGSEarthExplorer_mosaic.tif")
GEE_coded <- raster("Kruger_backdrop_mosaic.tif")
mpala <- raster("mpala_backdrop_mosaic.tif")
plot(mpala)
mpala[] <- 1
plot(mpala)

GEE_sub <- rasterFromCells(GEE, c(1:8), values=TRUE)
EE_sub <- rasterFromCells(EE, c(1:8), values = TRUE)
d_sub <- rasterFromCells(delta, c(1:8), values = TRUE)
d_sub_shift <- shift(d_sub, 15, 15)
plot(EE_sub)

plot(d_sub_shift, add = TRUE)

# SHould be true 
extent(d_sub_shift) == extent(EE_sub)

# Export a shifted Kruger GEE image
GEE_shift <- shift(GEE, 15, 15)
mpala_GEE_shift <- shift(mpala, 15, 15)
writeRaster(GEE_coded, "Kruger_backdrop_mosaic_shifted.tif")
writeRaster(mpala_GEE_shift, "mpala_GEE_shift.tif", overwrite = TRUE)

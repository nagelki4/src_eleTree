# Get the CHIRPS data for a park


library(raster)

#################################  Functions  ##################################################################
# Source the functions
source("F:/Dropbox/Permanent/Grad School/src_functions/src_masterfunctions.R")

# Set working directory to where CHIRPS is stored
setwd("F:/Dropbox/Permanent/Grad School/data/CHIRPS")

# How many years of data do you want?
n.years <- 2013-1984+1

# Folder with shape files of ground truth area
park.name <- "Mpala"
shape.folder <- paste0("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/ParkData/", park.name, "/Boundary")

# Read in boundary shapefile
park.boundary <- readOGR(shape.folder, paste0(park.name, "_Boundary"))

 



# List all the files from 1984-2013
chirps.list <- list.files(".", pattern = ".tif")


# Loop through and just keep the years you want
new.chirp <- c()
for(i in as.character(c(1984:2013))){
  temp <- grep(pattern = i, chirps.list, value = TRUE)
  new.chirp <- c(new.chirp, temp)
}


# Loop through, clip, take average and assign to table
# Build table first
mx <- matrix(nrow = n.years*12, ncol = 2)
chirps.df <- as.data.frame(mx)
names(chirps.df) <- c("date", "precip")

n <- 1
for(n in 1:length(new.chirp)){
  # Read in the raster
  prec.ras <- raster(new.chirp[n])
  
  # Clip that tif
  park.prec <- clipTIF(tifname = prec.ras, clipboundary = park.boundary)
  avg <- cellStats(park.prec, "mean")
  
  # Plug in the value
  chirps.df$date[n] <- substr(new.chirp[n], 11, 16)
  chirps.df$precip[n] <- avg 

}

















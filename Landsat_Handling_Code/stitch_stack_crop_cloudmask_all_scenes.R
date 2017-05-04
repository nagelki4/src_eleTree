#  DESCRIPTION  #
## This script stacks and crops all the scenes as part of the workflow to create the files for MESMA

# Date: 4/5/2017
# Ryan Nagelkirk

# Load libraries
library(raster)
library(rgdal)


##########################  Source the functions  ####################################################
source("F:/Dropbox/Permanent/Grad School/src_functions/src_masterfunctions.R")
#####################################################################################################

# start timer
start.time <- tick()

### FOR loop could start here if going through several parks

# Park name
park.name <- "Kruger"

# Set working directory
setwd(paste0("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/Landsat/", park.name))


# Where will the final stacked hdr go?
hdr.destination <- paste0("F:/Dropbox/Permanent/Grad School/Projects/EleTree/analysis/LandsatStacks/", park.name)

# Create the directory if it doesn't exist
if(!dir.exists(hdr.destination)){
  dir.create(hdr.destination)
}

# Load the shapefile
boundary.folder <- paste0("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/ParkData/", park.name, "/Boundary")
layer.name <- paste0(park.name, "_50kmBuffer")
clip.boundary <- readOGR(boundary.folder, layer.name)

# List the files
file.list <- list.files(".", pattern = "sr_band")
cloud.list <- list.files(".", pattern = "pixel_qa")
# cf_mask.list <- list.files(".", pattern = "cf_mask.tif")
file.list <- append(file.list, cloud.list)

# Create a list of unique names that can be iterated through. They will be used to find all files with the same date
landsat.names <- substr(file.list, 1, 25)
ls.unique <- unique(landsat.names)

# Get a list of the unique dates
dates <- substr(ls.unique, 18, 26) # Get the dates
unique.dates <- unique(dates) # Find unique dates

# i <- 1
# Now iterate through the dates, stitching, stacking and cropping all those on the same date
for(i in 1:length(unique.dates)){
  one.date.time <- tick()
  
  # Get the file names with the same date
  landsat.file.names <- grep(unique.dates[i], file.list, value = TRUE)
  
  
  ###########  CHECK IF FILE ALREADY EXISTS, NEXT IF IT DOES  ###################################################################
  # Create name to check (will also be used to name the file)
  # First, need the two raster prefixes
  landsat.prefixes <- unique(substr(landsat.file.names, 1, 40))
  
  # Create a text string for the different path and rows (will be part of the final name)
  pathrow.string <- ""
  
  for(n in 1:length(landsat.prefixes)){
    pathrow.string <- paste0(pathrow.string, "_", substr(landsat.prefixes[n], 11, 16)) # update path row string
  }
  
  # Create the final name
  final.hdr.name <- paste0(substr(landsat.prefixes[n], 1, 9), pathrow.string, substr(landsat.prefixes[n], 17, nchar(landsat.prefixes[n])), "_site_stack")
  print(paste0(i, " of ", length(unique.dates), ". File being created: ", final.hdr.name))
  
  # If the file exists, go to next in loop
  if(file.exists(paste0(hdr.destination, "/", final.hdr.name, ".hdr"))){
    next
  }
  
  #############  MERGE, CLIP, MASK, and STACK IMAGES  ##########################################################################
  
  # Run this nasty function. It will merge all the photos listed in landsat.file.names and stack them, then clip to boundary
  # and apply the could mask
  ras.stack <- mosaic.stack.clip.mask.Landsat2(landsat.file.names = landsat.file.names, clip.boundary = clip.boundary)
  
  ##############################################################################################################################  
  
  # Save the raster stack as an ENVI hdr file
  writeRaster(ras.stack, paste0(hdr.destination, "/", final.hdr.name), format = "ENVI", overwrite = TRUE)
  
  print("Time it took for one date of imagery:")
  print(tock(one.date.time))
  
  # Then remove the temp files that have been stored on the C drive. Hopefully everything but the rasters was stored on RAM
  removeTmpFiles()
}

tock(start.time)

# If you want to delete the temp folder, you can do it here
# unlink("temp", recursive = TRUE)








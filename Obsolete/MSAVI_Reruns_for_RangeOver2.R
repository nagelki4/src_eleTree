# MSAVI Reruns

# DESCRIPTION: This code checks each MSAVI2 file's range of values. If the range is more than 2 (which is shouldn't be),
#  the code reruns the MSAVI2 calculations to produce a new file in an attempt to fix the problem. 

# Set working directory
setwd("X:/shared_data/EastAfrica_Megatransect")

# Load Libraries
library(raster)
library(rgdal)
library(sp)
library(maps)
library(fields)

# List directories
dir.names <- list.dirs()

# Set up the file that will be used to list the bad files
bad.files <- list()

# Loop through and if the directory has MSAVI2_p or NDVI_p in the name, then it is an actual file. 
# Check to see what its value range is. 
# If the values are 0, then add that name to a list
# i <- dir.names[165]
row.counter <- 1
# j <- "MSAVI2_p168_r076_20140915"


for(i in dir.names){
  if(grepl("MSAVI2_", i) == TRUE ){ 
    file.list <- list.files(i, pattern = ".tif")
    # j <- file.list[1]
    for(j in file.list){
      full.name <- paste(i, "/", j, sep = "")
      x <- raster(full.name)
      y <- range(x)
      z <- y@data
      minimum <- slot(z, "min")
      maximum <- slot(z, "max")
      if(minimum == 0 | maximum == 0 | maximum - minimum > 2){
        # List that file's range
        bad.files[row.counter] <- paste(j, maximum - minimum, sep = "_")
        row.counter <- row.counter + 1
        # redo the MSAVI2 calc
        # get the park
        if(substr(i, 3, 4) == "Mp"){
          park <- "Mpala"
        } else if(substr(i, 3, 4) == "Se"){
          park <- "Serengeti"
        } else if (substr(i, 3, 4) == "Kr"){
          park <- "Kruger"
        } else if (substr(i, 3, 4) == "So"){
          park <- "South_Luangwa"
        } else 
          park <- "Ruaha"
        
        # Create the Landsat file name
        landsat.name <- paste("./", park, "/Landsat_", substr(j, 18, 21), "/Path", substr(j, 9, 11), "_Row", substr(j, 14, 16), "/", substr(j, 18, 25), sep = "")
        
        # Do the MSAVI calcs (overwrite)
        print(paste("Starting Mask and Indices calcs for folder", landsat.name))
        
        # Identify mask and band file paths (we just want 4 and 5)
        band4_path <- list.files(landsat.name, pattern = "band4", full.names = TRUE)
        band5_path <- list.files(landsat.name, pattern = "band5", full.names = TRUE)
        maskFile_path <- list.files(landsat.name, pattern = "cfmask.tif", full.names = TRUE) #two files have "cfmask" in them, so just use the order of files
        
        # Load the rasters
        mask <- raster(maskFile_path)
        band4 <- raster(band4_path)
        band5 <- raster(band5_path)
        
        # values below zero or >10000 are no good
        band4[band4 > 10000 | band4 < 0] <- NA
        band5[band5 > 10000 | band5 < 0] <- NA
        
        # Apply the mask to bands 4 and 5, setting cells with a mask value != 0 (clear) to NA (setting 0's to 1's and running this way saves about 4 sec, so 2:30ish overall)
        print("Applying Mask")
        mask[mask != 0] <- NA
        mask <- mask + 1
        band5 <- (band5 * mask)
        band4 <- (band4 * mask)
        
        # Stack the bands
        rstack <- stack(band5, band4)
        
        # Calc MSAVI2
        print("Starting MSAVI2")
        MSAVI2 <- calc(rstack, function(x) (2*x[,1] + 1 - sqrt((2*x[,1]+1)^2 - 8*(x[,1] - x[,2])))/2)
        
        # Write the files
        
        # Get the file names
        band4_name <- basename(band4_path) # this just makes things a little cleaner down the line
        band5_name <- basename(band5_path) # basename gets just the file name from the entire folder path
        
        # Get this info for naming the files
        year <- substr(band4_name, 10, 13) # pulls from the tif file name
        tile_n_date_text <- substr(landsat.name, nchar(park)+16, nchar(landsat.name)) # eg: "/Path168_Row076/20141204"
        path.num <- paste("p", substr(tile_n_date_text, 6, 8), sep = "")
        row.num <- paste("r", substr(tile_n_date_text, 13, 15), sep = "")
        image.date <- substr(tile_n_date_text, 17, 24)
        
        # Create the file paths for the processed files
        MSAVI2_filepath <- paste(park, "/MSAVI2_", year, sep = "")
        
        # Write rasters to file
        writeRaster(MSAVI2, paste(MSAVI2_filepath, "/", "MSAVI2_", path.num, "_", row.num, "_", image.date, sep = ""), format = "GTiff", overwrite = TRUE)
        
        # list the new range below the other
        x <- raster(paste("./", MSAVI2_filepath, "/", "MSAVI2_", path.num, "_", row.num, "_", image.date, ".tif", sep = ""))
        y <- range(x)
        z <- y@data
        minimum <- slot(z, "min")
        maximum <- slot(z, "max")
        
        # List that file's range
        bad.files[row.counter] <- paste(j, maximum - minimum, "new", sep = "_")
        row.counter <- row.counter + 1
        bad.files
      }
    }
  }
}
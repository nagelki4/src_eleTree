# Downloads CHIRPS data, saves originals (converted to tifs) to MyPassport (external hard drive), and clips out park boundaries



# Load libraries
library(gdalUtils)
library(utils)
library(R.utils)
library(raster)
library(rgdal)
library(RCurl)
library(maptools)
library(png)
library(rgeos)
library(sp)


### Notes:
# From HDR file: Projection= +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0


# Set variables and create folders
fileLocation <- "X:/nagelki4/Projects/EleTree"
url <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/bils/"
# chirps.folder <- paste(fileLocation, "/BaseData/CHIRPS", sep = "")
park.list <- c("Mpala") # this will need to be written differently, or could be, to just list the folders in ParkData

# Set working directory
setwd(fileLocation)

# Create CHIRPS folder in each park folder
for(n in park.list){
  park.chirp.folder <- paste("./ParkData/", n, "/CHIRPS", sep = "")
  dir.create(park.chirp.folder)
}


##### Read in the files
filenames <- getURL(url, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE)
filePaths <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep="")

# download files
for(k in filePaths[1]){
  if(grepl(pattern = "2016", k) == FALSE){ # this excludes all 2016 files
    download.file(k, basename(k), mode = "wb")
  }
}


##### Untar the bils and convert at tif
gz.filepaths <- list.files(pattern = ".gz")

for(i in gz.filepaths){
  # i <- gz.filepaths[1]
  untar(i)
  bil.name <- substr(i, 1, 16)
  bil <- raster(paste(bil.name, ".bil", sep = ""))
  bil[bil[] == -9999] <- NA
  proj4string(bil) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  tif.name <- paste(bil.name, ".tif", sep = "")
  writeRaster(bil, tif.name, format="GTiff",overwrite=TRUE)
  if(dir.exists("E:/nagelki4/Projects/2016/Elephants/Dissertation/CHIRPS") == TRUE){
    writeRaster(bil, paste("E:/nagelki4/Projects/2016/Elephants/Dissertation/CHIRPS/", bil.name, ".tif", sep = ""), format="GTiff",overwrite=TRUE)
  }
}


# This is for operating on the external hard drive's files to add the projection and get rid of NA's
# Was done just so didn't have to download all the files again
africa.tifs <- list.files("E:/nagelki4/Projects/2016/Elephants/Dissertation/CHIRPS", full.names = TRUE)
files.done <- 0
files.remaining <- length(africa.tifs)

for(k in africa.tifs){
  # k <- africa.tifs[51]
  tif <- raster(k)
  proj4string(tif) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  tif[tif[] == -9999] <- NA
  writeRaster(tif, k, format = "GTiff", overwrite = TRUE)
  files.done <- files.done + 1
  print(paste("Files remaining:",files.remaining - files.done))
}


##### Clip the park boundary 
tif.filepaths <- list.files(pattern = ".tif")

# For each park, clip the rasters 
for(j in park.list){
  # Set plot counter
  plot.counter <- 1
  
  # Identify folder to save clips
  clipped.folder <- paste(fileLocation, "/ParkData/", j, "/CHIRPS", sep = "")
  
  # Load the shapefile
  bound.folder <- paste("./ParkData/", j, "/Boundary", sep = "")
  bound.file <- list.files(bound.folder, pattern = ".shp") # get the name of the file
  bound.loc <- paste(bound.folder, "/", bound.file, sep = "")
  bd <- readShapePoly(bound.loc)
  proj4string(bd) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") # I just pulled this off the trmm file using projection(trmm)
  ext <- extent(bd)
  
  # Clip the park rasters
  for(m in tif.filepaths){
    chirp.raster <- raster(m)
    cr <- crop(chirp.raster, ext, snap = "out")
    
    # Save the raster
    writeRaster(cr, paste(clipped.folder, "/", j, "_", m, sep = ""), overwrite = TRUE)
    
    if(plot.counter == 1){
      overlay.folder <- paste("./ParkData/", j, "/CHIRPS/Overlay", sep = "")
      dir.create(overlay.folder)
      jpeg(paste(overlay.folder, "/", j, "_boundary_extent.jpg", sep = ""))
      plot(chirp.raster, ext = ext+1)
      plot(bd, add = T)
      dev.off()
      
      plot.counter <- plot.counter + 1
    }
    
  }
  
}



##### Compute stats

# Set the row counter for the df
row.counter <- 1

for(n in park.list){
  # n <- park.list[1]
  park.chirp.folder <- paste("./ParkData/", n, "/CHIRPS", sep = "")
  clip.list <- list.files(park.chirp.folder, pattern = "tif")
  for(o in clip.list){
    # Load the raster
    # o <- clip.list[1]
    clp <- raster(paste(park.chirp.folder, "/", o, sep = ""))
    
    # Compute the stats
    month.min <- clp@data@min
    month.max <- clp@data@max
    x <- as.array(clp)
    month.mean <- mean(x)
    all.values <- as.list(x)
    
    # Determine some stats
    year <- substr(o, 17, 20)
    month <- substr(o, 21,22)
    
    # Add the min, max and mean to a df
    if(row.counter == 1){
      # Create the df
      mmm <- matrix(NA, nrow = length(clip.list), ncol = 4)
      colnames(mmm) <- c("YearMo", "Min", "Max", "Mean")
      mmm.df <- data.frame(mmm)
      # Assign the values
      mmm.df$YearMo[row.counter] <- paste0(year, month)
      mmm.df$Min[row.counter] <- month.min
      mmm.df$Max[row.counter] <- month.max
      mmm.df$Mean[row.counter] <- month.mean
    }else{
      mmm.df$YearMo[row.counter] <- paste0(year, month)
      mmm.df$Min[row.counter] <- month.min
      mmm.df$Max[row.counter] <- month.max
      mmm.df$Mean[row.counter] <- month.mean
    }
    
    
    # Add all that month's values to a new column in a different df
    if(row.counter == 1){
      p <- data.frame(unlist(all.values))
      names(p)[1] <- paste0(year, month)
    }else{
      p$new <- unlist(all.values)
      names(p)[row.counter] <- paste0(year, month)
    }
    
    row.counter <- row.counter + 1
  }
  # Write out the dfs to file
  analysis.folder <- paste("./ParkData/", n, "/ANALYSIS", sep = "")
  write.csv(p, file = paste0(analysis.folder, "/CHIRPS_All.tile.monthly.cell.values.csv"), row.names = FALSE)
  write.csv(mmm.df, file = paste0(analysis.folder, "/CHIRPS_MinMaxMean.values.csv"), row.names = FALSE)
}




# Delete all the original files
message("Removing temporary files")
mapply(file.remove, list.files(pattern = "chirps"))



















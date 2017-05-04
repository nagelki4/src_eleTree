# Climate Stats


# This code goes through the Landsat files and finds the coordinates of each tile. Using those bounding coordinates, it extracts
# all the cells from a climate raster (TRMM, here) for every climate raster file. It then records all the cell values in that bounding area
# and also calculates the min, max and mean values, recording both to separate files that are printed as csv's in the folder that corresponds to that tile,
# along with all the clipped rasters


# Load libraries
require(raster)
require(rgdal)
require(sp)
require(maptools)
require(XML)


# Where are the climate files?
climate.dir <- "E:/nagelki4/Projects/2016/Elephants/Dissertation/TRMM"

# Set Working directory
setwd("X:/shared_data/EastAfrica_Megatransect")


# Identify the folders the code should be ran on
site_folders <- c("Kruger", "Mpala", "Ruaha", "Serengeti", "South_Luangwa")

for(j in site_folders){
  # j <- site_folders[2]
  
  
  # Create folders for the new files
  TRMM_folderpath <- paste(j, "/TRMM", sep = "")
  
  
  dir.create(TRMM_folderpath)
  
  
  # List all the directories in the folder 
  folderNames <- list.dirs(j)
  
  # f <- folderNames[8]
  
  # Set up variable to record the name of the path/row combination. Will be used to make sure the same tile isn't done twice
  tile.tracker <- ""
  
  # Loop through, running all operations only for unique tiles
  for (f in folderNames) {
    
    ## For the current folder, count the number of files with the LC identifier
    numLandsatFiles <- length(list.files(f, pattern = "LC8"))
    
    
    # If there are LC files present in the folder 
    if ((numLandsatFiles > 0) == TRUE){ 
      
      # get the row and path from the folder name
      tmp <- substr(f, nchar(f)-22, nchar(f)-9)
      
      # only if the row and path are different than before, proceed
      if(tile.tracker != tmp){
        
        # Immediately give tile.tracker the new name to prevent same tile from running again
        tile.tracker <- tmp
        
        # Open the xml doc, convert it to a list and get the bounding coordinates
        xml.name <- paste(f, "/", list.files(f, pattern = "xml"), sep = "")
        xml.doc <- xmlParse(xml.name)
        xml.list <- xmlToList(xml.doc)
        
        # Get corner coordinates
        w <- as.numeric(xml.list$global_metadata$bounding_coordinates$west)
        e <- as.numeric(xml.list$global_metadata$bounding_coordinates$east)
        n <- as.numeric(xml.list$global_metadata$bounding_coordinates$north)
        s <- as.numeric(xml.list$global_metadata$bounding_coordinates$south)
        # Make the extent object
        ext <- extent(w, e, s, n)
        
        # bring in the climate rasters and clip them all to the tile
        ## Create a folder with the path/row name
        pathrow.folder <- paste0(TRMM_folderpath, "/", tmp)
        dir.create(pathrow.folder)
        
        # Loop through all rasters
        climate.raster.list <- list.files(climate.dir, full.names = TRUE)
        # Set up a counter
        clim.counter <- 1
        for(k in climate.raster.list){
          # k <- climate.raster.list[1]
          
          # Pull the year and month
          dig.yr <- substr(basename(k), 6, 7)
          if(dig.yr < 20){
            year <- paste0("20", dig.yr)
          }else{
            year <- paste0("19", dig.yr)
          }
          month <- substr(basename(k), 8, 9)
          
          # Bring in the raster and crop
          ras <- raster(k)
          cr <- crop(ras, ext, snap = "out")
          # Create new file name
          cr.filename <- paste0(pathrow.folder, "/TRMM.", substr(basename(k), 0, nchar(basename(k))-9), tmp)
          writeRaster(cr, cr.filename, format="GTiff", overwrite = TRUE)
          
          
          # Compute the stats
          month.min <- cr@data@min
          month.max <- cr@data@max
          x <- as.array(cr)
          month.mean <- mean(x)
          all.values <- as.list(x)
          
          # Add the min, max and mean to a df
          if(clim.counter == 1){
            # Create the df
            mmm <- matrix(NA, nrow = length(climate.raster.list), ncol = 4)
            colnames(mmm) <- c("YearMo", "Min", "Max", "Mean")
            mmm.df <- data.frame(mmm)
            # Assign the values
            mmm.df$YearMo[clim.counter] <- paste0(year, month)
            mmm.df$Min[clim.counter] <- month.min
            mmm.df$Max[clim.counter] <- month.max
            mmm.df$Mean[clim.counter] <- month.mean
          }else{
            mmm.df$YearMo[clim.counter] <- paste0(year, month)
            mmm.df$Min[clim.counter] <- month.min
            mmm.df$Max[clim.counter] <- month.max
            mmm.df$Mean[clim.counter] <- month.mean
          }
          
          
          # Add all that month's values to a new column in a different df
          if(clim.counter == 1){
            p <- data.frame(unlist(all.values))
            names(p)[1] <- paste0(year, month)
          }else{
            p$new <- unlist(all.values)
            names(p)[clim.counter] <- paste0(year, month)
          }
          #Update the counter
          clim.counter <- clim.counter + 1
        }
        # Write out the dfs to file
        write.csv(p, file = paste0(pathrow.folder, "/All.tile.monthly.cell.values.csv"), row.names = FALSE)
        write.csv(mmm.df, file = paste0(pathrow.folder, "/MinMaxMean.values.csv"), row.names = FALSE)
        
      }
    }
  }
}


# Now go through and clip the park boundaries, while also creating new tables


#' Download and convert TRMM data
#' 
#' @author Ryan Nagelkirk (large portions based on script by Claudia Vitolo)
#' 
#' @description The TRMM dataset provide global historical rainfall estimation in a gridded format.  
#' 
#' @param fileLocation file path where to save the GeoTiff
#' @param url url where data is stored (e.g. "ftp://disc2.nascom.nasa.gov/data/TRMM/Gridded/3B43_V7/2012/")
#' @param product this is the code that identifies a product, default is "3B43"
#' @param version this is the version number, default is 7
#' @param year year of interest, default is 2012
#' @param type this is the type of information needed, default is "precipitation.accum". Other types could be "gaugeRelativeWeighting.bin" and "relativeError.bin"
#' @param BBlonMin Minimum latitude of bounding box
#' @param BBlonMax Maximum latitude of bounding box
#' @param BBlatMin Minimum longitude of bounding box
#' @param BBlatMax Maximum longitude of bounding box
#' 
#' @return Data is loaded as rasterbrick, then converted to a multilayer Geotiff that can 
# be opened in any GIS software.
#' 
#' @details This code is based upon Martin Brandt's blog post: 
# http://matinbrandt.wordpress.com/2013/09/04/automatically-downloading-and-processing-trmm-rainfall-data/
#' and on the TRMM FAQ: http://disc.sci.gsfc.nasa.gov/additional/faq/precipitation_faq.shtml
#' 
#' @examples 
#' trmm(fileLocation="~/",url="ftp://disc2.nascom.nasa.gov/data/TRMM/Gridded/",product="3B43",version=7,year=2012,BBlonMin=-3.82,BBlonMax=-3.63,BBlatMin=52.43,BBlatMax=52.52)
#'


# trmm <- function(fileLocation = "~/",
#                  url = "ftp://disc2.nascom.nasa.gov/data/TRMM/Gridded/",
#                  product = "3B43",
#                  version = 7,
#                  year = c(1998:2015),
#                  type = "precipitation.accum",
#                  BBlonMin = NULL,
#                  BBlonMax = NULL,
#                  BBlatMin = NULL,
#                  BBlatMax = NULL
# ){



fileLocation = "X:/nagelki4/Projects/EleTree"
url = "ftp://disc2.nascom.nasa.gov/data/TRMM/Gridded/"
product = "3B43"
version = 7
year = c(1998:2015)
type = "precipitation.accum"
BBlonMin = NULL
BBlonMax = NULL
BBlatMin = NULL
BBlatMax = NULL
bound.folder <- "Boundary"
trmm.folder <- "TRMM"
park.list <- c("Mpala") # this could be made to list the folders in the ParkData folder


require(raster)
require(rgdal)
require(RCurl)
require(maptools)
require(png)
require(rgeos)
require(sp)


setwd(fileLocation)

# Create TRMM folder in each park folder
for(n in park.list){
  park.trmm.folder <- paste("./ParkData/", n, "/TRMM", sep = "")
  dir.create(park.trmm.folder)
}

# 1. Write for loop to go through years
for (i in year){
  # i <- 2010
  myURL <- paste(url, product, "_V", version, "/", i, "/", sep="")
  # by default this is 
  # myURL <- "ftp://disc2.nascom.nasa.gov/data/TRMM/Gridded/3B43_V7/2012/"
  
  filenames <- getURL(myURL, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE)
  filePaths <- paste(myURL, strsplit(filenames, "\r*\n")[[1]], sep="")
  
  # the following line allows to download only files with a certain pattern,
  # e.g. only certain months or days. 
  # "*precipitation.accum" means monthly accumulated rainfall here.
  selectedfilePaths <- filePaths[grep(filePaths, pattern=paste("*",type,sep=""))] 
  
  # download files
  for(k in selectedfilePaths){
    # k <- selectedfilePaths[1]
    download.file(k, basename(k), mode = "wb")
  }
  
  # 2. Create the raster stack or brick
  #    Go through each file, rasterize it, then stack them all as one variable
  #   Will have to put the file names in order so that the first one is january
  
  
  # Might just have to multiply the precip data by the number of hours in that month
  
  #       j <- sort(basename(selectedfilePaths))[8]
  #       j <- "3B43.120101.7.precipitation.bin"
  
  for (j in sort(basename(selectedfilePaths))){
    
    # trmm template
    trmm <- raster(xmn=-180, xmx=+180, ymn=-50, ymx=50, ncol=1440, nrow=400)
    trmm[] <- readBin(j, 'double', n=576000, size=4, endian='big')
    # normalize (North up, -180.. 180)
    x <- flip(trmm, direction =  'y')
    # write to geotif to have entire file
    writeRaster(x, paste(j, ".tif", sep = ""), format="GTiff", overwrite = TRUE)
    
    if(dir.exists("E:/nagelki4/Projects/2016/Elephants/Dissertation/TRMM") == TRUE){
      writeRaster(x, paste("E:/nagelki4/Projects/2016/Elephants/Dissertation/TRMM/", j, ".tif", sep = ""), format="GTiff", overwrite=TRUE)
    }
    
  } 
  
}



for(m in park.list){ # this isn't completed to go through each park
  plot.counter <- 1 # this is for printing one image of the park overlaid on TRMM pixels and for row number in df
  
  # Load the shapefile. This can be looped later for each park
  bound.file <- list.files(paste("./ParkData/", m, "/", bound.folder, sep = ""), pattern = ".shp") # get the name of the file
  bound.loc <- paste("./ParkData/", m, "/", bound.folder, "/", bound.file, sep = "")
  bd <- readShapePoly(bound.loc)
  proj4string(bd) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") # I just pulled this off the trmm file using projection(trmm)
  plot(bd) 
  ext <- extent(bd)
  
  
  # Clip it from the precip data and project the shapefile on a larger portion of the precip data
  clipped.folder <- paste("./ParkData/", m, "/TRMM", sep = "")
  
  # List the TRMM files
  trmm.files <- list.files(pattern = ".tif")
  # t <- trmm.files[1]
  
  # Draw files from the global file and clip them and write those files to the "clipped" folder
  for(t in trmm.files){
    trmm <- raster(t)
    cr <- crop(trmm, ext, snap = "out")
    
    # Save the raster
    writeRaster(cr, paste(clipped.folder, "/", m, "_", t, sep = ""), overwrite = TRUE) 
    
    # Plot overlay if it is the first file for that park
    if(plot.counter == 1){
      overlay.folder <- paste("./ParkData/", m, "/TRMM/Overlay", sep = "")
      dir.create(overlay.folder)
      jpeg(paste(overlay.folder, "/", m, "_boundary_extent.jpg", sep = ""))
      plot(trmm, ext = ext+1)
      plot(bd, add = T)
      dev.off()
      plot.counter <- plot.counter + 1
    }
  }
}


# Compile the stats and print out

# Set the row counter for the df
row.counter <- 1

for(n in park.list){
  # n <- park.list[1]
  park.trmm.folder <- paste("./ParkData/", n, "/TRMM", sep = "")
  clip.list <- list.files(park.trmm.folder, pattern = "tif")
  for(o in clip.list){
    # Load the raster
    # o <- clip.list[1]
    clp <- raster(paste(park.trmm.folder, "/", o, sep = ""))
    
    # Compute the stats
    month.min <- clp@data@min
    month.max <- clp@data@max
    x <- as.array(clp)
    month.mean <- mean(x)
    all.values <- as.list(x)
    
    # Determine some stats
    year <- paste(if(substr(o, 12, 13) > 15){19} else {20}, substr(o, 12, 13), sep = "" )
    month <- substr(o, 14,15)
    
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
  write.csv(p, file = paste0(analysis.folder, "/TRMM_All.tile.monthly.cell.values.csv"), row.names = FALSE)
  write.csv(mmm.df, file = paste0(analysis.folder, "/TRMM_MinMaxMean.values.csv"), row.names = FALSE)
}







# Delete all the original files
message("Removing temporary files")
mapply(file.remove, list.files(pattern = "accum"))







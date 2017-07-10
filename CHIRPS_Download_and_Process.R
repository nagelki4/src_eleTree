# Downloads CHIRPS data and saves the data as tifs. Code checks and gets only files that haven't been downloaded.
#  1. Downloads zipped files 
#  2. Unzips, projects and stores as tifs 

# Ryan Nagelkirk
# 20170530

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
zipFolder <- "E:/nagelki4/data/CHIRPS"
tifFolder <- "F:/Dropbox/Permanent/Grad School/data/CHIRPS"
url <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/bils/"
# url <- "ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/bils/" # potential alternate site


# Set working directory
setwd(zipFolder)


##### Read in the files
filenames <- getURL(url, ftp.use.epsv = FALSE, ftplistonly=TRUE, crlf=TRUE)
filePaths <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep="")
filePaths <- filePaths[-1] # the first path is a folder ("sub/"), which don't want

# Get the files that have already been downloaded and unzipped
downloaded.files <- list.files(zipFolder, pattern = ".tar.gz")
unzipped.files <- list.files(tifFolder, pattern = "v2p0")


### THIS DIDN'T WORK SO JUST DOWNLOADED USING DOWNTHEMALL IN FIREFOX
# download files that aren't already in download folder
k <- filePaths[1]
for(k in filePaths){
  # get the relevant portion of the name
  zip.name <- substr(k, nchar(k)-22, nchar(k))
    if(zip.name %in%  downloaded.files == FALSE){ # If the zipped website name isn't in the list of downloaded names, download the file
      download.file(k, basename(k), mode = "wb")
    }
}



##### Untar the bils and convert to tif only if not already done
gz.filepaths <- list.files(pattern = ".gz") 

for(i in gz.filepaths){
  # i <- gz.filepaths[1]
  # Use the tar ball name to create what the tif name would be if it existed
  unzip.name <- paste0(substr(i, nchar(i)-22, nchar(i)-7), ".tif")
  if(unzip.name %in%  unzipped.files == FALSE){ # If the zipped website name isn't in the list of downloaded names, download the file
    untar(i)
    bil.name <- substr(i, 1, 16)
    bil <- raster(paste(bil.name, ".bil", sep = ""))
    bil[bil[] == -9999] <- NA
    proj4string(bil) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    writeRaster(bil, paste(tifFolder, "/", unzip.name, sep = ""), format="GTiff",overwrite=TRUE)
  }
}


# Delete all the bil and hdr files that were unzipped but not needed anymore
# Get list of files to delete
message("Removing temporary files")
mapply(file.remove, list.files(pattern = "\\.hdr$|\\.bil$"))


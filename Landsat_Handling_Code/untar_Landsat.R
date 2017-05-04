# Landsat File Organizer and Tar.gz Unzipper
# 4/5/2017
# Ryan Nagelkirk

# This code determines what folder the newly downloaded Landsat file should go to, then moves it and unzips it in that location



#################################  Functions  ##################################################################
# Source the functions
source("F:/Dropbox/Permanent/Grad School/src_functions/src_masterfunctions.R")

# start timer
start.time <- tick()

# Go to the location of the landsat files
# setwd("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/Landsat/Originals")
setwd("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/Landsat/Originals")

# List the files
landsat.files <- list.files(".", pattern = ".gz")

# What is the folder with all the different landsat locations listed? i.e. where are the folders being created for each park?
parent.landsat.folder <- "F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/Landsat"

# What are the unique pathrow combos?
unique(substr(landsat.files, 5, 10))



# Make a counter for printing
counter <- 0


# Loop through, determining what folder they should be in and unzipping as it goes
for(i in landsat.files){
  
  # Update counter
  counter <- counter + 1
  print(paste(counter, "of", length(landsat.files), "files")) # Displays the count
  
  # Determine what folder they should be going in and unzip
  # KRUGER
  if(grepl("169076", i) | grepl("169077", i) | grepl("168076", i) | grepl("168077", i) | grepl("168078", i)){
    dest.folder <- paste0(parent.landsat.folder, "/Kruger")
    # Now run the unzip function to move it there and unzip
    landsatUntar(landsat.file = i, dest.folder.name = dest.folder)
  }
  
  # TSAVO EAST
  if(grepl("167062", i)){
    dest.folder <- paste0(parent.landsat.folder, "/Tsavo_East")
    # Now run the unzip function to move it there and unzip
    landsatUntar(landsat.file = i, dest.folder.name = dest.folder)
  }
  
  # QWE NP
  if(grepl("173060", i)){
    dest.folder <- paste0(parent.landsat.folder, "/QWE")
    # Now run the unzip function to move it there and unzip
    landsatUntar(landsat.file = i, dest.folder.name = dest.folder)
  }
  
  # CHOBE
  if(grepl("174072", i) | grepl("174073", i) | grepl("173072", i) | grepl("173073", i)){
    dest.folder <- paste0(parent.landsat.folder, "/Chobe")
    # Now run the unzip function to move it there and unzip
    landsatUntar(landsat.file = i, dest.folder.name = dest.folder)
  }
  
  # SELOUS
  if(grepl("167065", i) | grepl("167066", i) | grepl("167067", i) | grepl("167068", i) |
     grepl("166065", i) | grepl("166066", i) | grepl("166067", i) | grepl("166068", i)){
    dest.folder <- paste0(parent.landsat.folder, "/Selous")
    # Now run the unzip function to move it there and unzip
    landsatUntar(landsat.file = i, dest.folder.name = dest.folder)
  }
  
  # TULI
  if(grepl("170075", i) | grepl("170076", i)){
    dest.folder <- paste0(parent.landsat.folder, "/Tuli")
    # Now run the unzip function to move it there and unzip
    landsatUntar(landsat.file = i, dest.folder.name = dest.folder)
  }
  # Then remove the temp files that have been stored on the C drive. Hopefully everything but the rasters was stored on RAM
  removeTmpFiles()
}

tock(start.time)





















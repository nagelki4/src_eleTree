# Landsat File Organizer and Tar.gz Unzipper
# 6/9/2016
# Ryan Nagelkirk

# Creating code to:
#                  1. organize Landsat images by date, row and path
#                  2. unzip the tar.gz files in the new location

# Load packages
library(date)

# Set working directory
setwd("/mnt/research/ersamlab/shared_data/EastAfrica_Megatransect")

# Create list of files and folders
site_folders <- c("Kruger", "Mpala", "Ruaha", "Serengeti", "South Luangwa")
#print ("here")
#break
# Organize files into folders named using the dates of the files (YYYYMMDD)
# After moving files into folder, unzip them

for (j in site_folders){
  print(j)
  # Create list of files
  fileList <- list.files(j, pattern = ".gz")
  numFiles <- length(fileList)
  
  
  for(i in 1:numFiles){
    # Use the date values to create Landsat_'year' folders and insert into those 
    # other folders with names in YYYYMMDD format within which to unzip the files
    
    
    ## Get the year
    year_num <- substr(fileList[i], 10, 13)
    DOY_num <- as.numeric((substr(fileList[i], 14,16)))
    path_num <- substr(fileList[i], 4, 6)
    row_num <- substr(fileList[i], 7, 9)
    
    # Use the year and DOY value to create date
    originVal <- paste(year_num,"-01-01", sep = "") # origin needed in as.Date() function. Set as first day of that year
    hyphenDate <- as.Date(DOY_num - 1, origin = originVal) # subract 1 because 0 is first day 
    
    # Get the day and month numbers from date
    month_num <- substr(hyphenDate, 6,7)
    day_num <- substr(hyphenDate, 9,10)
    
    # Create folder and file names
    LandsatFolderName <- paste(j, "/Landsat_", year_num, sep = "")
    PathRowFolderName <- paste(LandsatFolderName, "/Path", path_num, "_Row", row_num, sep = "")
    newFolderName <- paste(PathRowFolderName, "/", year_num, month_num, day_num, sep = "")
    newFileName <- paste(newFolderName, "/", fileList[i], sep = "")
    oldFileName <- paste(j, "/", fileList[i], sep = "")
    
    # Check to see if the year folder exists and create if necessary
    # Does the Landsat folder exist?
    if (dir.exists(LandsatFolderName) == FALSE){
      dir.create(LandsatFolderName)
    }
    
    # Does the Path and Row folder exist?
    if (dir.exists(PathRowFolderName) == FALSE){
      dir.create(PathRowFolderName)
    }
    
    if (dir.exists(newFolderName) == FALSE) { # if folder doesn't exist, create it and move file
      dir.create(newFolderName)
      # Move file
      file.copy(oldFileName, newFileName) # or do file.rename to cut and paste
      untar(newFileName, exdir = newFolderName)
    } else { # else just move the file to the appropriate folder
      file.copy(oldFileName, newFileName)
      untar(newFileName, exdir = newFolderName)
    }
    # Delete the tar.gz file after unzipping (saves space)
    file.remove(newFileName) 
  
  }
}






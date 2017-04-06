# Landsat File Organizer and Tar.gz Unzipper
# 4/5/2017
# Ryan Nagelkirk

# This is a simple function to just create an unzipped folder and move the unzipped files into it



#################################  Functions  ##################################################################
# Source the functions
source("X:/nagelki4/src_functions/src_masterfunctions.R")

# Where is the landsat imagery and where do you want the unzipped images to go?
landsat.folder <- "X:/nagelki4/Projects/EleTree/data/Landsat/Mpala/Landsat_EleAnalysis"
unzip.folder.name <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/data/Landsat/mpala"

## Run the landsatUntar function
landsatUntar(landsat.folder = landsat.folder, unzip.folder.name = unzip.folder.name)








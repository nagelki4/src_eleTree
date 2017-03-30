# This is supposed to calculate the tree cover in a 30 x 30 grid cell to use as test data for
# my Landsat spectral unmixing classification

## GENERAL NOTES
#    - It looks like water is going to be a problem. Trees are often classified as water for the Google images bc they are RGB
#     - Code is written to have an option of classifying water.


#############################  LOAD LIBRARIES  ######################################################################
library(raster)
library(rgdal)
library(jpeg)
library(ggmap)
library(ggplot2)
library(RgoogleMaps)
library(png)
library(maps)
library(fields)
library(sp)


#################################  Functions  ##################################################################
# Source the functions
source("X:/nagelki4/src_functions/src_masterfunctions.R")

#################################  WORKSPACE  ##############################################################
# Load the old workspace, if needed
# load("X:/nagelki4/Projects/EleTree/src/R Workspaces/SMA_workspace.Rdata")

#################################  SMA  ##################################################################

## MASTER SWITCH for SMA checking
SMA.check.mode <- TRUE

# Folder with SMA results
SMA.folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/MESMA/Viper/20170315_AdditionalRuns/hdr_results/"
SMA.14 <- "Laikipia_mesma_unconstrained_10percent_20170315_DarkTree_TS"
SMA.87 <- "87_Laik_20161203.tif"
save.plot <- TRUE
plot.save.folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/MESMA/Viper/20170315_AdditionalRuns/images/lm_addition/"
MESMA.version <- c("Laik_50_150_", "unconstrained_")
secondary.version <- "DarkTree_TS"
total.mesma.name <- paste0(MESMA.version[2], secondary.version)
is.hdr <- TRUE
is.unconstrained <- T
TS_Only <- TRUE



#######################  Section for automated plotting to eval the SMA hdr results  ######################

# get list of hdr files
hdr.list <- list.files(SMA.folder, pattern = ".hdr")
hdr.list <- hdr.list[!grepl(pattern = "class", hdr.list)] # exclude the hdr files of the classifications 


# Create a table to plug hdr.name, RMSE, slope, 1-slope, Adj r-sq, yint into
mx <- matrix(0, nrow = length(hdr.list), ncol = 6)
c.names <- c("hdr.name", "RMSE", "adj.r.sq", "slope", "1-slope", "y-int")
colnames(mx) <- c.names
hdr.eval.df <- as.data.frame(mx)



for(h in 1:length(hdr.list)){
  # Get file name
  SMA.14 <- substr(h, 0, nchar(hdr.list[h])-4) # Take off the .hdr, which isn't needed

  total.mesma.name <- gsub(pattern = "%", "prcnt", SMA.14) # remove any % signs
  is.hdr <- TRUE # this is always true for this loop
  
  # Determine if unconstrained
  if(grepl(pattern = "uncon", SMA.14)){
    is.unconstrained <- TRUE
  }else{is.unconstrained <- FALSE}
  
  # Determine if TS
  if(grepl(pattern = "TS", SMA.14)){
    TS_Only <- TRUE
  }else{TS_Only <- FALSE}

#####################################

# Set the band order for cover types in the Laikipia SMA
# 1987
tr.bandnum.87 <- 2 # KEY 1203  2  noncon  2   1120  
so.bandnum.87 <- 1 #           1          1  
gr.bandnum.87 <- 3 #           3          3


if(TS_Only == TRUE){ # the code is being funny, so running grass as a duplicate of soil. Won't be plotted
  # 2014
  tr.bandnum.14 <- 1 #           1          1         2
  gr.bandnum.14 <- 2
  so.bandnum.14 <- 2 #           2          2         3
  
}else{
  # 2014
  tr.bandnum.14 <- 1 #           1          1         2
  gr.bandnum.14 <- 2 #           2          2         3 
  so.bandnum.14 <- 3 #           3          3         1
}


######################################  GENERAL VARIABLES  ###################################################

# Define variables
sample.size <- 500 # how many points should go into the ground truth? 
get.Google.image <- FALSE  # Should the Google image be downloaded? (Set to FALSE if they're already downloaded)
classify.water <- FALSE  # Should water be included as a condidate class?
classify.google.and.record <- FALSE # should the Google images be classified and the cover recorded? 
ndvi_n_msavi <- FALSE # calc NDVI and MSAVI2?
vcf_landsat <- FALSE  # load the 2015 VCF?
plot.9by9 <- FALSE  # Should an image with the 9x9 cells be created for each rep?
save.confusion.matrix <- FALSE  # Save the confusion matrix?
save.transition.matrix <- FALSE  # Save the transition matrix?
run.confuse.matrix <- FALSE
run.transition.matrix <- FALSE

# Turn off the unnecessary parts of the code if just checking SMA
if(SMA.check.mode == TRUE){
  get.Google.image <- FALSE
  classify.google.and.record <- FALSE
  vcf_landsat <- FALSE
  ndvi_n_msavi <- FALSE
  run.confuse.matrix <- FALSE
  run.transition.matrix <- FALSE
}

###############################  DIRECTORIES  ##############################################################
# Landsat Folder
landsat.folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/SMA/Landsat/Mpala/"

# Location and prefixes of the 2014 and 1987 imagery
image_prefix <- c("LC81680602014034LGN00", "LT51680601987056XXX01")
image_folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/data/Landsat"

# Working directory and where outputs are saved 
working.dir <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/SMA/Rasters"
google.image.folder <- "test_images_20170102"  # Downloaded Google images store here

# Landsat VCF file path
vcf <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/Landsat_VCF/p168r060_TC_2015.tif"

# Folder with shape files of ground truth area
shape.folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/ParkData/Mpala/Boundary"



#############################  SET WORKING DIRECTORY  ######################################################################
# Set working directory to where the imagery should be saved
setwd(working.dir)
# par(mfrow = c(1,1)) # just in case needed for changing later
par(mar=c(1,1,2,1))


if(SMA.check.mode == FALSE){ # if these aren't already in the environment, will need to run for SMA check
  #####################  SHAPEFILE & RASTERS & POINT GENERATION  ############################################
  
  ###### Read in Landsat and shapefiles
  # Landsat
  stack.2014 <- stackLandsat(path = image_folder, prefix = image_prefix[1])
  stack.1987 <- stackLandsat(path = image_folder, prefix = image_prefix[2])
  # Shapefiles
  mpala.boundaries <- readOGR(shape.folder, "MpalaImageDelineated")
  mpala.boundary.simple <- readOGR(shape.folder, "MpalaShapeforGoog")
  # Convert Mpala shapefile to lat long, which everything else is in
  mpala.boundaries <- spTransform(mpala.boundaries, crs(stack.2014))
  mpala.boundary.simple <- spTransform(mpala.boundary.simple, crs(stack.2014))
  plot(mpala.boundary.simple)
  plot(mpala.boundaries, add = TRUE)
  # Select the third one, since that is the one I have a rule set for right now
  ground.truth.frame <- as.SpatialPolygons.PolygonsList(mpala.boundaries@polygons[3], crs(mpala.boundaries))
  plot(ground.truth.frame)
  
  # Plot the RGBs from 1987 and 2014
  cr.stack.2014 <- crop(stack.2014, extent(mpala.boundary.simple)) # take down to extent of park
  crop.stack.2014 <- mask(cr.stack.2014, mpala.boundary.simple) # clip by park boundary
  rgbLandsat(stackname = crop.stack.2014, r = 5, b = 3)
  
  cr.stack.1987 <- crop(stack.1987, extent(mpala.boundary.simple)) # take down to extent of park
  crop.stack.1987 <- mask(cr.stack.1987, mpala.boundary.simple) # clip by park boundary
  rgbLandsat(stackname = crop.stack.1987, r = 3, b = 1)
  
  
  # Make blank background using Landsat cfmask
  mpala_cfmask <- crop.stack.2014[[1]]
  plot(mpala_cfmask, axes=F,box = F, legend = FALSE) # 0 is cloud free, 1 == cloud
  
  
  ###### POINT GENERATION ###############################
  # Crop the area that will be used for random point generation. This could be done several
  # times if using more polygons in the future.
  
  # Clip out the raster and set the values to 1
  plot(mpala_cfmask, axes=F,box = F)
  plot(ground.truth.frame, axes=F,box = F, add = TRUE)
  sample.raster <- mask(mpala_cfmask, ground.truth.frame) 
  sample.raster[!is.na(sample.raster)] <- 1
  plot(sample.raster, axes=F,box = F)
  
  
  
  
  ######  GENERATE LIST OF POINTS  ############################################################################
  # Create list of cells in Mpala for sampling
  # First, how many cells have a value?
  cell.num <- length(na.omit(sample.raster[])) # this used to be na.omit(mpala[])
  
  # Use that to assign new values to the cells (numbering them)
  cell.val <- cell.num # this is easier to understand
  sample.raster[!is.na(sample.raster)] <- 1:cell.val
  plot(sample.raster, axes=F,box = F)
  
  # Randomly select cells from the raster (the cells are represented as a list of numbers at this point)
  set.seed(1) # set this for now. Can change later
  sample.cells <- sample(1:cell.val, sample.size)
  
  ######  PLOT THE POINTS  #########
  ### Plot the cells that were chosen 
  # Get the cell numbers
  cell.numbers <- c()
  for(i in sample.cells){
    sing.cell <- Which(sample.raster == i, cells= TRUE)
    cell.numbers <- c(cell.numbers, sing.cell)
  }
  
  # Assign the sample points 1000000 and then set rest to NA
  trial <- sample.raster
  trial[cell.numbers] <- 1000000
  trial[trial < 1000000] <- NA
  
  # Convert cells to points and add them to the plot
  trial.points <- rasterToPoints(trial)
  par(mar=c(1,1,2,1)) 
  plot(mpala_cfmask, axes=F,box = F, legend = FALSE, breaks = c(0,1))
  plot(ground.truth.frame, add = TRUE)
  new <- sample(1:500, 50) # get what is likely the hand calibrated points, though this wasn't varified (meant for illustration, not analysis)
  hand.points <- trial.points[new, ]
  points(trial.points, pch = 0, cex = .3)
}


#########  GET GOOGLE IMAGERY  #####################################################################
# Only tested for single image, but should work!
if(get.Google.image){
  # Create the google image folder if not already done
  if(file.exists(paste0("./", google.image.folder)) == FALSE){
    dir.create(paste0("./", google.image.folder))
  }
  # Get the Google image for each pixel
  for(t in 1:sample.size){
    # Create the image name for that pixel
    image.name <- paste0("./", google.image.folder, "/pixel_", cell.numbers[t], ".png")
    # GET THE IMAGE FROM GOOGLE!! 
    # Written by Ryan. Function in src_masterfunctions.R
    j <- getGoogleimage(rastername = sample.raster, ras.pixel.num = cell.numbers[t], 
                        destfilename = image.name, typeofmap = "satellite", zoomlevel = 19)
  }
}



#######  CLASSIFY and RECORD GOOGLE COVER DATA  #################################################
master.df.csv.name <- "./default_master.csv"

if(classify.google.and.record){
  # This will bring in the images, crop them, classify, and record to the master.df and the nine.cell.df (then saves the nine.cell)
  #Function in src_masterfunctions.R
  master.df <- CreateCoverTable(samplesize = sample.size, googleimagefolder = google.image.folder, 
                                cellnumbers = cell.numbers, save.pixel.rgb = FALSE, 
                                ninepixCSVname = "./nine_pix_df_20170305.csv", masterdfname = master.df.csv.name)
  # The default of TGSW function (within CreateCoverTable) is not to save the classified image
}



########  SMA  ##################################################################################
# Read in the master.df
master.df <- read.csv(master.df.csv.name)

# Run the giant function to add SMA
sma.dat <- addSMA(SMAfolder = SMA.folder, tiffname = SMA.14, treeband = tr.bandnum.14, grband = gr.bandnum.14,
                  soilband = so.bandnum.14, parkboundary = mpala.boundary.simple, groundtruthboundary = ground.truth.frame, 
                  df = master.df, isHDR = is.hdr, is.unconstrained = is.unconstrained)

master.df <- sma.dat[[1]]

######## NDVI and MSAVI2  #######################################################################
if(ndvi_n_msavi){
  # Calculate the indices
  MSAVI2 <- MSAVI2_Landsat_calc(path = image_folder, prefix = image_prefix[1]) # this could be made FASTER if both didn't have to mask
  NDVI <- NDVI_Landsat_calc(path = image_folder, prefix = image_prefix[1])  # plus should only calc for the park, not whole scene
  # Clip to sample area
  ndvi <- crop(NDVI, extent(mpala.boundary.simple))
  msavi <- crop(MSAVI2, extent(mpala.boundary.simple))
  ndvi <- mask(ndvi, ground.truth.frame)
  msavi <- mask(msavi, ground.truth.frame)
  # Write rasters to file
  writeRaster(NDVI, "NDVI", format = "GTiff")
  writeRaster(MSAVI2, "MSAVI2", format = "GTiff")
  
  # Add the data to master.df
  # Function in src_masterfunctions.R
  master.df <- addData(tiffname = ndvi, df = master.df, colname = "NDVI") 
  master.df <- addData(tiffname = msavi, df = master.df, colname = "MSAVI2")
}

########  VCF  ##########################
if(vcf_landsat){
  vcf.file <- raster(vcf)
  vcf.file <- crop(vcf.file, extent(mpala.boundary.simple))
  vcf.tree <- mask(vcf.file, ground.truth.frame)
  vcf.tree <- vcf.tree/100
  # Add the data to master.df
  # Function in src_masterfunctions.R
  master.df <- addData(tiffname = vcf.tree, df = master.df, colname = "VCF")
}


if(SMA.check.mode == FALSE){
  write.csv(master.df, "master.df_FULL_20170305.csv")
}

## So when looking at new SMA results, should just run the addSMA function, then three 1:1 plot functions (tree, grass, soil)
# Write in a variable at the top as Checking.SMA.mode. When it is true, it sets all the conditions for doing stuff to false


###########  CONFUSION MATRIX ############################################################

if(run.confuse.matrix == TRUE){
  # Function in src_masterfunctions.R
  confusion.mtx <- confusionMatrix(df = master.df) # can change settings to save the mtx and whether water is included
}


####################  CREATE 1:1 PLOT  #############################################################

# Plot the 3 plots
# Change the margins back to the default
par(mar=c(5.1,4.1,4.1,2.1))
    
# Trees
# Function in src_masterfunctions.R
plot1to1(x = master.df$nine.tree, y = master.df$raw.SMA.tree, xlab = "Observed % Tree Cover", ylab = "Raw Predicted % Tree Cover", 
         main = "Percent Tree Cover: Observed (9-cell) vs. Raw SMA", ylim = c(-100, 200), add_one2one_line = TRUE, add.reg.line = TRUE,
         save.plot = save.plot, save.plot.name = paste0(plot.save.folder, "Tree_SMAraw_", total.mesma.name, ".png"))

# Grass
if(TS_Only == FALSE){
  plot1to1(x = master.df$nine.grass, y = master.df$raw.SMA.grass, xlab = "Observed % Grass Cover", ylab = "Raw Predicted % Grass Cover", 
           main = "Percent Grass Cover: Observed (9-cell) vs. Raw SMA", ylim = c(-100, 200), add_one2one_line = TRUE, add.reg.line = TRUE,
           save.plot = save.plot, save.plot.name = paste0(plot.save.folder, "Grass_SMAraw_", total.mesma.name, ".png"))
}


# Soil
plot1to1(x = master.df$nine.soil, y = master.df$raw.SMA.soil, xlab = "Observed % Soil Cover", ylab = "Raw Predicted % Soil Cover", 
         main = "Percent Soil Cover: Observed (9-cell) vs. Raw SMA", ylim = c(-100, 200), add_one2one_line = TRUE, add.reg.line = TRUE,
         save.plot = save.plot, save.plot.name = paste0(plot.save.folder, "Soil_SMAraw_", total.mesma.name, ".png"))

####################  PLUG IN RMSE etc  #################################################################



####################  TRANSITION MATRIX  ################################################################
if(run.transition.matrix){
  #### THIS ISN'T fully ready to run. Need to create stacks of the recent and old cover (14 and 87) in TGS order.
  ## Those will go into the function below
  recent.stack <- stack(sma.dat[2:4])
  old.stack <- stack() # this needs to be filled
  
  # Function in src_masterfunctions.R
  transition.matrix <- create.trans.mtx(recent.TGS.cover.stack = recent.stack, yearofrecent = "2014", 
                                        old.cover.TGS.cover.stack = old.stack, yearofold = "1987",
                                        save.transition.matrix = FALSE) # if TRUE, can set the name (tran.mtx.name)
}


###################################################################################################
} # This is the end of the hdr plotting loop














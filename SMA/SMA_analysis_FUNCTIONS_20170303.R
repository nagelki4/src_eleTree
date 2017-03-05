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

#################################  VARIABLES  ##################################################################
# Define variables
sample.size <- 500 # how many points should go into the ground truth? 
water.frac.thresh <- 0.05 # When water occupies what fraction of the scene will it be included in the classification? This was done because trees are sometimes classified as water, so only class water when it's fairly certain it's in the image. 
get.Google.image <- FALSE  # Should the Google image be downloaded? (Set to FALSE if they're already downloaded)
Google.image.dim.final <- 30 # in meters, the resolution of the final Google image (after clipping) (This can be made into list of the different resolutions to analyze at. So could do accuracy assessment for 30 meters and then 120 as well, to see how compare)
save.rgb.pixel <- F  # Should the clipped Google image be saved?
save.class.pixel <- FALSE  # Save the classified Google image?
classify.water <- FALSE  # Should water be included as a condidate class?
save.confusion.matrix <- FALSE  # Save the confusion matrix?
save.transition.matrix <- FALSE  # Save the transition matrix?
check.ground.truth <- FALSE  # If true, this will run the ground truth hand classification code. Should always be FALSE
classify.google.and.record <- FALSE # should the Google images be classified and the cover recorded? 
ndvi_n_msavi <- FALSE # calc NDVI and MSAVI2?
vcf_landsat <- FALSE  # load the 2015 VCF?
three_by_three <- TRUE # should the area be 3x3 landsat pixels for classification?
one_by_one <- FALSE  # sets classified area to a single cell of size Google.image.dim.final (30m)
plot.9by9 <- FALSE  # Should an image with the 9x9 cells be created for each rep?
is.hdr <- FALSE


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

# Folder with SMA results
SMA.folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/SMA/Results/20170108_TreeRemade_LaikipiaZeros/tif/"
SMA.14 <- "20170108_TreeRemade_1billionConstraint_Laikipia_Zeros_SMA.tif"
SMA.87 <- "87_Laik_20161203.tif"


# Set the band order for cover types in the Laikipia SMA
# 1987
tr.bandnum.87 <- 2 # KEY 1203  2  noncon  2   1120  
so.bandnum.87 <- 1 #           1          1  
gr.bandnum.87 <- 3 #           3          3
# 2014
tr.bandnum.14 <- 1 #           1          1         2
so.bandnum.14 <- 2 #           2          2         3 
gr.bandnum.14 <- 3 #           3          3         1

# Folder with shape file of ground truth area
shape.folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/ParkData/Mpala/Boundary"




#############################  SET WORKING DIRECTORY  ######################################################################
# Set working directory to where the imagery should be saved
setwd(working.dir)
# par(mfrow = c(1,1)) # just in case needed for changing later
par(mar=c(1,1,2,1))


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
plot(TREE, axes=F,box = F)
plot(ground.truth.frame, axes=F,box = F, add = TRUE)
sample.raster <- mask(TREE, ground.truth.frame) 
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
master.df.csv.name <- "./default_master"

if(classify.google.and.record){
  # This will bring in the images, crop them, classify, and record to the master.df and the nine.cell.df (then saves the nine.cell)
  #Function in src_masterfunctions.R
  master.df <- CreateCoverTable(samplesize = sample.size, googleimagefolder = google.image.folder, 
                                cellnumbers = cell.numbers, save.pixel.rgb = FALSE, 
                                ninepixCSVname = "./nine_pix_df_20170305.csv", masterdfname = master.df.csv.name)
}




## So when looking at new SMA results, should just run the addSMA function, then three 1:1 plot functions (tree, grass, soil)





############  INPUT SMA, NDVI, MSAVI and VCF  ###################################################



########  SMA  ###########################

# Read in the master.df
master.df <- read.csv(master.df.csv.name)

# Run the giant function to add SMA
master.df <- addSMA(SMAfolder = SMA.folder, tiffname = SMA.14, treeband = tr.bandnum.14, grband = gr.bandnum.14,
                       soilband = so.bandnum.14, parkboundary = mpala.boundary.simple, groundtruthboundary = ground.truth.frame, 
                       df = master.df, isHDR = is.hdr)






######## NDVI and MSAVI2  ###############
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
}

########  VCF  ##########################
if(vcf_landsat){
  vcf.file <- raster(vcf)
  vcf.file <- crop(vcf.file, extent(mpala.boundary.simple))
  vcf.tree <- mask(vcf.file, ground.truth.frame)
  # Add the data to master.df
}













   




if(ndvi_n_msavi){
  master.df$NDVI[t] <- ndvi[cell.numbers[t]]
  master.df$MSAVI2[t] <- msavi[cell.numbers[t]]
}


write.csv(master.df, "master.df_20170116.csv")




###########  CONFUSION MATRIX ############################################################

# (This will need to be modified to include water. Should be done just based on column names)
# Don't hard code the numbers, just the names. If water, then 6 names, if not, then 5.
# Just that information should be enough for the rest to run.
# That is started with the if statement below, but not continued through


if(classify.water){
  c.name <- c("Trees", "Grass", "Soil", "Water", "Total", "Percent Correct")
}else{c.name <- c("Trees", "Grass", "Soil", "Total", "Percent Correct")}
      
# make the table
c.mtx <- matrix(0, nrow = 5, ncol = 5) # ncol should be length(c.name)
c.mtx.df <- as.data.frame(c.mtx)
colnames(c.mtx.df) <- c.name
rownames(c.mtx.df) <- c.name

# n <- 1
for(n in 1:sample.size){
  truth <- master.df$maj.truth[n]
  pred <- master.df$maj.SMA[n]
  
  # Assign the coordinates for plugging into table
  if(truth == "Trees"){
    truth.cor <- 1
  } else if (truth == "Grass"){
    truth.cor <- 2
  } else if (truth == "Soil"){
    truth.cor <- 3
  }
  
  if(pred == "Trees"){
    pred.cor <- 1
  } else if (pred == "Grass"){
    pred.cor <- 2
  } else if (pred == "Soil"){
    pred.cor <- 3
  }
  
  # Now go the position in the table and add 1
  c.mtx.df[truth.cor, pred.cor] <- c.mtx.df[truth.cor, pred.cor] + 1
}

# Now sum the columns and calc the percent correct
# First define how many columns/rows have numbers to add
summing.values <- ncol(c.mtx.df) - 2
tot.rowcol <- ncol(c.mtx.df) - 1
per.rowcol <- ncol(c.mtx.df)
tot.correct <- 0 # set up total correct variable to be adding values to from the diagonal 

# Now calculate them
for(i in 1:summing.values){
  c.mtx.df[i, tot.rowcol] <- sum(c.mtx.df[i, 1:summing.values]) # does the Total column
  c.mtx.df[i, per.rowcol] <- c.mtx.df[i, i] / c.mtx.df[i, tot.rowcol] # does the percent column
  c.mtx.df[tot.rowcol, i] <- sum(c.mtx.df[1:summing.values, i]) # does the Total row
  c.mtx.df[per.rowcol, i] <- c.mtx.df[i, i] / c.mtx.df[tot.rowcol, i] # does the percent column
  
  tot.correct <- tot.correct + c.mtx.df[i, i]
  # Fill in the total/total cell and the overall percent correct 
  if(i == summing.values){
    # Fill in the Total/Total combo cell
    c.mtx.df[tot.rowcol, tot.rowcol] <- sum(c.mtx.df[1:summing.values, tot.rowcol])
    # Last, fill in the total percent correct (lower right corner cell)
    c.mtx.df[per.rowcol, per.rowcol] <- tot.correct / c.mtx.df[tot.rowcol, tot.rowcol]
  }
}

if(save.confusion.matrix){
  write.csv(c.mtx.df, "accuracy_matrix.csv")
} 

####################  CREATE 1:1 PLOT  #############################################################

# Plot the 3 plots
# Change the margins back to the default
par(mar=c(5.1,4.1,4.1,2.1))
    
# TREES
# Calculate Stats
##   RMSE
tree.rmse <- sqrt( mean( (master.df$SMA.tree- master.df$nine.tree)^2 , na.rm = TRUE ) )
##   R-squared
y.mean <- mean(master.df$SMA.tree)
y.line <- master.df$nine.tree
y <- master.df$SMA.tree
SE.y.mean <- sum((y - y.mean)^2) # what is the standard error from the mean of y? (SMA tree cover)
SE.y.line <- sum((y - y.line)^2) # what is the standard error from the 1:1 line? (Google tree cover)
tree.r.sq <- 1 - SE.y.line/SE.y.mean # NOTE 1 (see bottom) 
correl <- cor(master.df$nine.tree, master.df$SMA.tree)
covar <- cov(master.df$nine.tree, master.df$SMA.tree)
# Plot
plot(master.df$nine.tree*100, master.df$SMA.tree*100, 
     main = paste0("Percent Tree Cover: Observed (9-cell) vs. SMA \nRMSE = ", round(tree.rmse, 2)*100, "%  R-squared = ", round(tree.r.sq, 2), " Cor = ", round(correl, 2)), 
     xlab = "Observed % Tree Cover", ylab = "Predicted % Tree Cover", xlim = c(0,100), ylim = c(0,100))
abline(0,1) # Add the 1:1 line


# GRASS
# Calculate Stats
## RMSE
grass.rmse <- sqrt( mean( (master.df$SMA.grass- master.df$nine.grass)^2 , na.rm = TRUE ) )
## R-squared
y.mean <- mean(master.df$SMA.grass)
y.line <- master.df$nine.grass
y <- master.df$SMA.grass
SE.y.mean <- sum((y - y.mean)^2) # what is the standard error from the mean of y? (SMA grass cover)
SE.y.line <- sum((y - y.line)^2) # what is the standard error from the 1:1 line? (Google grass cover)
grass.r.sq <- 1 - SE.y.line/SE.y.mean
correl <- cor(master.df$nine.grass, master.df$SMA.grass)
covar <- cov(master.df$nine.grass, master.df$SMA.grass)
# Plot
plot(master.df$nine.grass*100, master.df$SMA.grass*100, main = paste0("Percent Grass Cover: Observed vs. Predicted \nRMSE = ", round(grass.rmse, 2)*100, "%  R-squared = ", round(grass.r.sq, 2), " Cor = ", round(correl, 2)), xlab = "Observed % Grass Cover", ylab = "Predicted % Grass Cover", xlim = c(0,100), ylim = c(0,100))
abline(0,1) # Add the 1:1 line


# SOIL
# Calculate Stats
## RMSE
soil.rmse <- sqrt( mean( (master.df$SMA.soil- master.df$nine.soil)^2 , na.rm = TRUE ) )
## R-squared
y.mean <- mean(master.df$SMA.soil)
y.line <- master.df$nine.soil
y <- master.df$SMA.soil
SE.y.mean <- sum((y - y.mean)^2) # what is the standard error from the mean of y? (SMA soil cover)
SE.y.line <- sum((y - y.line)^2) # what is the standard error from the 1:1 line? (Google soil cover)
soil.r.sq <- 1 - SE.y.line/SE.y.mean
correl <- cor(master.df$nine.soil, master.df$SMA.soil)
covar <- cov(master.df$nine.soil, master.df$SMA.soil)
# Plot
plot(master.df$nine.soil*100, master.df$SMA.soil*100, main = paste0("Percent Soil Cover: Observed vs. Predicted \nRMSE = ", round(soil.rmse, 2)*100, "%  R-squared = ", round(soil.r.sq, 2), " Cor = ", round(correl, 2)), xlab = "Observed % Soil Cover", ylab = "Predicted % Soil Cover", xlim = c(0,100), ylim = c(0,100))
abline(0,1) # Add the 1:1 line





####################  TRANSITION MATRIX  ################################################################

# Calcs the dominant cover in these, too, because it goes through all the pixels

# make the table
t.mtx <- matrix(0, nrow = 3, ncol = 3)
tran.mx <- as.data.frame(t.mtx)
c.name <- c("Trees", "Grass", "Soil")
colnames(tran.mx) <- c.name
rownames(tran.mx) <- c.name
# 
# # Create the entire list of cell numbers that have values
# # Get the cell numbers
# all.cell.val <- na.omit(mpala[])
# 
# all.cell.numbers <- c()
# for(i in all.cell.val){
#   sing.cell.new <- Which(mpala == i, cells= TRUE)
#   all.cell.numbers <- c(all.cell.numbers, sing.cell.new)
# }



# # Check whether there are NA's within Mpala at all cover levels by summing them and if anything is still 0, then it was NA at all
# t87 <- TREE.87
# g87 <- GRASS.87
# s87 <- SOIL.87
# t87[is.na(t87)] <- 0
# g87[is.na(g87)] <- 0
# s87[is.na(s87)] <- 0
# # Checked out. Would want to do this for the 14 year too, but thought not necessary
# sum87 <- t87 + g87 + s87
# plot(sum87)


# Stack to see if they are the same
n.cov.stack <- stack(TREE.87, GRASS.87, SOIL.87)
plotRGB(n.cov.stack, scale = 255)
n.cover.stack <- stack(TREE, GRASS, SOIL, TREE.87, GRASS.87, SOIL.87, mpala)

# Go to each cell in the two years and figure out dominant class
# To start, set NA's to 0 so comparisons can be ran
t87 <- TREE.87
g87 <- GRASS.87
s87 <- SOIL.87
t14 <- TREE
g14 <- GRASS
s14 <- SOIL
t87[is.na(t87)] <- 0
g87[is.na(g87)] <- 0
s87[is.na(s87)] <- 0
t14[is.na(t14)] <- 0
g14[is.na(g14)] <- 0
s14[is.na(s14)] <- 0
# n <- tran.sample[1]

# Doing all the cells 
for(n in 1:length(mpala)){

  # Compute percentages and what the majority cover is
  tree_87 <- t87[n]
  grass_87 <- g87[n]
  soil_87 <- s87[n]
  tree_14 <- t14[n]
  grass_14 <- g14[n]
  soil_14 <- s14[n]
  
  # Define major cover types and skip to next loop if none is greater than the others (means they were all NA originally)
  # Define majority cover for 1987 SMA
  if(tree_87 > grass_87 & tree_87 > soil_87){
    cov.87 <- "Trees"
  } else if(grass_87 > tree_87 & grass_87 > soil_87){
    cov.87 <- "Grass"
  } else if(soil_87 > tree_87 & soil_87 > grass_87){
    cov.87 <- "Soil"
  } else{next}
  
  # Define major cover for 2014 SMA
  if(tree_14 > grass_14 & tree_14 > soil_14){
    cov.14 <- "Trees"
  } else if(grass_14 > tree_14 & grass_14 > soil_14){
    cov.14 <- "Grass"
  } else if(soil_14 > tree_14 & soil_14 > grass_14){
    cov.14 <- "Soil"
  } else{next}
  
  
  # Assign the table coordinates (row/column) for plugging into table
  if(cov.87 == "Trees"){
    coord.87 <- 1
  } else if (cov.87 == "Grass"){
    coord.87 <- 2
  } else if (cov.87 == "Soil"){
    coord.87 <- 3
  }
  
  if(cov.14 == "Trees"){
    coord.14 <- 1
  } else if (cov.14 == "Grass"){
    coord.14 <- 2
  } else if (cov.14 == "Soil"){
    coord.14 <- 3
  }
  
  # Now go to the position in the table and add 1
  tran.mx[coord.14, coord.87] <- tran.mx[coord.14, coord.87] + 1
}

if(save.transition.matrix){
  write.csv(tran.mx, "transition_matrix.csv")
}

###################################################################################################
# CREATE CHANGE MAPS
# Subtract the three maps to get the change

# Tree cover change
tree.delta <- t14/255 - t87/255
tree.delta[tree.delta[] == 0] <- NA
plot(tree.delta, main = "Tree cover change")

# Grass cover change
grass.delta <- g14/255 - g87/255
grass.delta[grass.delta[] == 0] <- NA
plot(grass.delta, main = "Grass cover change")

# Soil cover change
soil.delta <- s14/255 - s87/255
soil.delta[soil.delta[] == 0] <- NA
plot(soil.delta, main = "Soil cover change")

















## NOTES ####################
#  1. This is an inappropriate use of R-squared. We aren't trying to see how well GE tree cover predicts SMA tree cover. But it's a familiar metric, and I get that we want it to be close to 1, so I'm including it for now. In all, it would mean "How much of the variation in the SMA cover can be explained by the variation in the GE cover (the 1:1 line). That isn't the point of what I'm doing and that is why it's an inappropriate use. I'm not trying to understand how SMA tree cover can be explained by GE cover. I'm just tring to see how well they match, and for that reason, the RMSE is the better measure.  










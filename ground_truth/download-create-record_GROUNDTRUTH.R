#################################  CHECKING GROUND TRUTH BY HAND  ##############################################

#  This code downloads the Google Imagery and saves it. Then that imagery is available to classify by hand.
#  The threshold values are manually saved in a table by the person classifying. 


#################################  Load Libraries  #############################################################
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
source("F:/Dropbox/Permanent/Grad School/src_functions/src_masterfunctions.R")

#################################  Workspace  ##################################################################
# Load the old workspace
# If this code creates a workspace, then it could be loaded here. load("X:/nagelki4/Projects/EleTree/R Workspaces/.RData")


#################################  Variables  ##################################################################
park <- "Mpala"
truth.point.num <- 500
hand.truth.num <- 50
landsat.image <- "LC81680602014034LGN00_cfmask.tif"
hand.truth.image.folder <- "manual_ground_truth_images"
ground.truth.image.folder <- "automated_ground_truth_images"
pixel.size.meters <- 30



#################################  Control Panel  ##############################################################
get.Google.image <- FALSE
save.rgb.pixel <- TRUE
save.class.pixel <- TRUE
save.final.class.df <- FALSE

#############################  Set Working Directory  ##########################################################
# Set working directory to where the imagery will be saved, along with the final table
setwd(paste0("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/ParkData/", park, "/ground_truth"))
# par(mfrow = c(1,1)) # just in case needed for changing later
par(mar=c(1,1,2,1))



############################  Create DFs  ######################################################################
# DF for manual classification entries
mx <- matrix(0, nrow = hand.truth.num, ncol = 7)
c.names <- c("Count", "park.pxl.num", "sample.pxl.num", "hand.tree", "hand.grass", "hand.soil", "truth.maj.cover")
colnames(mx) <- c.names # above, pxl.num is the pixel number within the sample area, park.pxl.num is from the entire park
hand.truth.df <- as.data.frame(mx)

# DF for automated classification
mx <- matrix(0, nrow = truth.point.num, ncol = 7)
c.names <- c("Count", "park.pxl.num", "pxl.num", "hand.tree", "hand.grass", "hand.soil", "maj.truth")
colnames(mx) <- c.names # above, pxl.num is the pixel number within the sample area, park.pxl.num is from the entire park
auto.truth.df <- as.data.frame(mx)



#############################  Create Points  #################################################################
###### Read in Landsat and shapefiles
# Landsat
landsat.image <- raster(landsat.image)

# Shapefiles
google.boundary <- readOGR("./gis", "MpalaImageDelineated")
park.boundary <- readOGR("./gis", "MpalaShapeforGoog")
# Convert Mpala shapefile to lat long, which everything else is in
google.boundary <- spTransform(google.boundary, crs(landsat.image))
park.boundary <- spTransform(park.boundary, crs(landsat.image))
plot(park.boundary)
plot(google.boundary, add = TRUE)
# Select the section of the park to develop rule set for
ground.truth.frame <- as.SpatialPolygons.PolygonsList(google.boundary@polygons[3], crs(landsat.image))
plot(ground.truth.frame)


# Create background Park image
blank.park <- clipTIF(tifname = landsat.image, clipboundary = google.boundary)
blank.park[!is.na(blank.park)] <- 0
# plot(blank.park, col = "lightgray")



#############################  Point Generation  ###############################################################
# Crop the area that will be used for random point generation. This could be done several
# times if using more polygons in the future.

# Clip out the raster and set the values to 1
sample.area.raster <- clipTIF(tifname = blank.park, clipboundary =  ground.truth.frame) 
sample.area.raster[!is.na(sample.area.raster)] <- 1
plot(sample.area.raster, axes=F,box = F)


plot(blank.park)
plot(sample.area.raster)
plot(blank.park, add = TRUE)

####  Generate List of Points 
# Create list of cells in Mpala for sampling
# First, how many cells have a value?
cell.vals <- length(na.omit(sample.area.raster[])) # this used to be na.omit(mpala[])

# Use that to assign new values to the cells (numbers them)
sample.area.raster[!is.na(sample.area.raster)] <- 1:cell.vals
plot(sample.area.raster, axes=F,box = F)

# Randomly select cells from the raster (the cells are represented as a list of numbers at this point)
set.seed(1) # set this for now. Can change later
sample.cells <- sample(1:cell.vals, truth.point.num)

####  Plot the Points
# First, get the cell numbers
# This is done because the entire raster is bigger than just the sample area. Outside the area,
# there are cells with NA values that are white in the plot. So have to find the cell number in the
# whole sample area raster, not just in the area that has values. 
sample.cell.numbers <- c()
for(i in sample.cells){
  sing.cell <- Which(sample.area.raster == i, cells= TRUE) # the sample.area.raster values aren't actual cells, but just cells values
  sample.cell.numbers <- c(sample.cell.numbers, sing.cell)
}

# Switch the sample area to NA and then give the sample points their pixel number value
trial <- sample.area.raster
trial[] <- NA
trial[sample.cell.numbers] <- sample.cell.numbers # assign values for the cells based on their pixel number
plot(trial)

# Convert cells to points and add them to the plot
hand.points <- rasterToPoints(trial) # make the ground truth hand classified points
par(mar=c(1,1,2,1)) 
plot(blank.park, axes=F,box = F, legend = FALSE, col = "lightgray")
plot(ground.truth.frame, add = TRUE)
points(hand.points, pch = 0, cex = .3)


#############################  Park/Sample Area Cell Number Redundancy  ##############################################
#####  Get the cell numbers for the raster of the whole park (will be used in naming and added to hand.check.df)
#####  This is done for redundancy - so two different types of indexing are possible. So if park boundary shp file ever changes, 
#####  can just the sample area shp file to index. 
# Extract cell numbers
park.cell.nums <- extract(x = blank.park, y = hand.points[, 1:2], cellnumbers = TRUE) # this is what was wanted
park.cell.nums[, 2] <- hand.points[, 3] # reattach the pixel numbers from the sample area
# Set those points to high value and set all else to NA
park.ras <- blank.park
park.ras[] <- NA
park.ras[park.cell.nums[, 1]] <- park.cell.nums[, 1]

# Convert the remaining cells to points
park.points <- rasterToPoints(park.ras) # these points will match the hand.points above (they do)
# Compare the points
plot(hand.points, pch = 0, cex = .1)
points(park.points, pch = 0, cex = .1) # nothing should change when these points are added (nothing does)

## NOTE: If you plot using the cell numbers, they will be slightly offset. I checked this rigorously, and it's something
# to do with the plot function/display, not the actual data. Using the cell numbers yields the same coordinates whether it's
# from the whole park or from the sample area (using the correct cell numbers for each, of course) and using either 
# type of cell number (park- or sample area-derived) results in the very same image being pulled from Google. So there should 
# be no problem using either the park or sample area cell numbers when comparing to MESMA results, so long as, again, the 
# MESMA data has been clipped to each's value-derived areas (park boundary or sample area boundary, respectively)


#############################  Restore Cell Number Order  #########################################################

# First, get park.cell.nums back in the original order of sample.cell.numbers so we can use it and the first 50
# will still be distributed randomly
# First make into a df
cellnums.df <- as.data.frame(park.cell.nums)
names(cellnums.df) <- c("park_cell", "sample_cell")

# Order to match sample.cell.numbers ordering
cellnums.df <- cellnums.df[match(sample.cell.numbers, cellnums.df$sample_cell),]
rownames(cellnums.df) <- 1:nrow(cellnums.df) # order the row names so they aren't confusing


#############################  Create Image Name Lists  ###########################################################
# Create lists of the files to crop (auto and hand)
auto.images <- c()
for(t in 1:truth.point.num){
  # Create the image name for that pixel
  image.name <- paste0("./", ground.truth.image.folder, "/pixel_park", cellnums.df$park_cell[t], 
                       "_sample", cellnums.df$sample_cell[t], "_", park, ".png")
  auto.images <- c(auto.images, image.name)
}

# Create hand image list
hand.images <- c()
for(t in 1:hand.truth.num){
  image.name.new <- paste0("./", hand.truth.image.folder, "/pixel_park", cellnums.df$park_cell[t], 
                           "_sample", cellnums.df$sample_cell[t], "_", park, ".png")
  hand.images <- c(hand.images, image.name.new)
}

# Create the hand image 30x30 rgb name list
rgb.3030.names <- c()
for(x in 1:length(hand.images)){
  rgb.3030.name <- paste0(substr(hand.images[x], 1, nchar(hand.images[x]) - 4), "_3030_rgb.png")
  rgb.3030.names <- c(rgb.3030.names, rgb.3030.name)
}

# Create the google image folders if not already done
if(file.exists(paste0("./", ground.truth.image.folder)) == FALSE){
  dir.create(paste0("./", ground.truth.image.folder))
}

if(file.exists(paste0("./", hand.truth.image.folder)) == FALSE){
  dir.create(paste0("./", hand.truth.image.folder))
}



#############################  Get Google Imagery  #################################################################
if(get.Google.image){
  
  # Get the Google image for each pixel
  for(t in 1:truth.point.num){
    # Create the image name for that pixel
    image.name <- auto.images[t] # this looks risky, but the naming above follows this same order
    # GET THE IMAGE FROM GOOGLE!! 
    # Function in src_masterfunctions.R
    j <- getGoogleimage(rastername = sample.area.raster, ras.pixel.num = cellnums.df$sample_cell[t], 
                        destfilename = image.name, typeofmap = "satellite", zoomlevel = 19)
  }
  
  # Move the first 50 images to the hand classifying folder (yes, they are now in random order (a good thing: it's the original order from random number generator))
  for(t in 1:hand.truth.num){
    image.name <- auto.images[t]
    image.name.new <- hand.images[t]
    file.copy(image.name, image.name.new)
  }
}



#############################  Create 30x30 Shapefile  ##########################################################
# This isn't currently used to crop the images because it was noticeably slower than the crop.google line
cell.size <- pixel.size.meters/190 # 190m is the length of one side of the Google image
half.pix <- cell.size / 2
center <- .5 # Middle of the image. The reference point for indexing

# Create the boundaries of the upper left cell. The 9-pixel loop will modify 
# these as it counts through the cells (extent.list)
bottom <- center - half.pix
right <- center + half.pix
top <- center + half.pix 
left <- center - half.pix # might be able to simplify these. Write whole loop first

#Create extent object 
cr.ext <- extent(c(left, right, bottom, top))

# Coerce extent to a SpatialPolygons object and add to plot
cell.poly <- as(cr.ext, 'SpatialPolygons')



#############################  Crop to 30x30 Meters  ##########################################################
## Loop through and crop the hand images. Later code can do the same for the auto.images
for(x in 1:length(hand.images)){
  # Crop the image (function in src_masterfunctions.R)
  img <- crop.google(image.name = hand.images[x], pixel.res = pixel.size.meters)
  
  # Plot and save the RGB image of the cell - not needed if they are already saved
  if(save.rgb.pixel){
    png(rgb.3030.names[x]) # Save the image
    plotRGB(img, scale = 1)
    dev.off()
  }
}





#############################  START of MANUAL CLASSIFICATION  ###################################################
image.num <- 1 # Manually set this as you go

#############################  Index Calculations  ###############################################################
# Read in the single cell and landscape images
current.png <- readPNG(rgb.3030.names[image.num])
landscape.png <- readPNG(hand.images[image.num])

# Pull out the red, green and blue bands from both images
red.c <- raster(current.png[,,1]) # the "c" is just a reminder that it's the color value for the cropped image
green.c <- raster(current.png[,,2])
blue.c <- raster(current.png[,,3])

red <- raster(landscape.png[,,1]) 
green <- raster(landscape.png[,,2])
blue <- raster(landscape.png[,,3])

# Stack so can create images later
rgb.stack <- stack(red.c, green.c, blue.c)
landscape.stack <- stack(red, green, blue)

# Sum the RGB values. Low values are dark areas which correspond to trees. 
sum.im.pre <- green.c + red.c + blue.c

# Calculate the Green-Red Vegetation Index
grvd.pre <- (green.c - red.c) / (green.c + red.c)

# Calculate summary stats for images to determine if it is mostly grass or soil
#  This is done because images with a lot of grass or soil require different thresholds for classifying tree cover

# Find the percent area of the cell that has a brightness sum over 2
bright <- red.c + green.c + blue.c
bright.area <- bright > 2
bright.frac <- length(bright.area[bright.area == 1]) / length(bright.area)


# Look at pixels with higher red values than green
red.area <- red.c > green.c
red.frac <- length(red.area[red.area == 1]) / length(red.area) 



# Below, classifier is able to set thresholds as they like. At end, all thresholds along with every index and measure 
# are recorded in the df.That way, can analyze the values (simple regression) to see if there are relationships between
# the thresholds chosen and the different brightness values etc.



#############################  Classify Landcover  ###############################################################
# First, plot the image of the overall area with the cell overlaid
plotRGB(landscape.stack, scale = 1)
plot(cell.poly, add = TRUE)

# Do trees, then grass. Soil will automatically be calculated after grass (soil = remaining pixels)
# TREES 
tree.bright.thresh <- 1.4
trees <- sum.im.pre < tree.bright.thresh
tree.equation <- paste0("sum.im.pre < ", tree.bright.thresh)
# Compare classification to image
par(mar=c(1,1,2,1))
plot(trees) # this is done twice to overlay the rgb image in the second plot
plotRGB(rgb.stack, scale = 1, add = TRUE)
plot(trees) # tree cover will be green


# Now remove the tree pixels from the image that will go into identifying soil and grass
# create tree mask
tree.mask <- trees
tree.mask[tree.mask == 1] <- 50
holy.image <- grvd.pre + tree.mask
holy.image.3 <- sum.im.pre + tree.mask
holy.image[holy.image > 5] <- NA
holy.image.3[holy.image.3 > 5] <- NA
grvd <- holy.image
sum.im <- holy.image.3


# GRASS
# Split soil and grass using GRVD and brightness. Soil is nonveg and bright
grass.bright.thresh <- 2.1
grass.grvd.thresh <- -0.05
grass <-  sum.im < grass.bright.thresh & grvd > grass.grvd.thresh
grass.equation <- paste0("sum.im < ", grass.bright.thresh, " & grvd > ", grass.grvd.thresh)

# Compare classification to image
plot(grass) 
plotRGB(rgb.stack, scale = 1, add = T)
plot(grass) # grass will now be green

# SOIL 
# Soil is the rest
soil <- grass == 0
soil.equation <- "remainder"



#############################   Combine Covers  #################################################################
# Give the different cover types different values
grass.ad <- grass
grass.ad[grass.ad == 1] <- 2
grass.ad[is.na(grass.ad)] <- 0

soil.ad <- soil # don't need to set soil to a different value. Just let it be 1
soil.ad[is.na(soil.ad)] <- 0

tree.ad <- trees
tree.ad[tree.ad == 1] <- 3
tree.ad[is.na(tree.ad)] <- 0


# Combine
tot.img <- tree.ad + soil.ad + grass.ad
tot.img[tot.img == 0] <- NA

# One last comparison
plotRGB(landscape.stack, scale = 1)
plot(cell.poly, add = TRUE)
par(mar=c(1,1,2,1)) #plotRGB makes the margins small, so have to set it back to this
plot(tot.img)
plotRGB(rgb.stack, scale = 1, add = T)
plot(tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil")

# Plot and save final classification (if wanted)
if(save.class.pixel){ # This will overwrite already existing files (the creation date and time doesn't change, though)
  class.3030.name <- paste0(substr(hand.images[x], 1, nchar(hand.images[x]) - 4), "_3030_classified.png")
  # Save the image
  png(class.3030.name)
  plot(tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil")
  dev.off()
}


#############################   Calculate Cover %  ###################################################################
# Calculate the % covers
# Get the number of cells in each
tr.cells <- length(tot.img[tot.img == 3])
gr.cells <- length(tot.img[tot.img == 2])
sl.cells <- length(tot.img[tot.img == 1])
tot.cells <- ncell(tot.img)

# Compute percentages and what the majority cover is
p.tree <- tr.cells / tot.cells
p.grass <- gr.cells / tot.cells
p.soil <- sl.cells / tot.cells


# Define majority cover for Google classification
if(p.tree > p.grass & p.tree > p.soil){
  maj.cov <- "Trees"
} else if(p.grass > p.tree & p.grass > p.soil){
  maj.cov <- "Grass"
} else{maj.cov <- "Soil"}



#############################  Record Values  ###################################################################
hand.truth.df$Count[image.num] <- image.num
hand.truth.df$park.pxl.num[image.num] <- cellnums.df$park_cell[image.num]
hand.truth.df$sample.pxl.num[image.num] <- cellnums.df$sample_cell[image.num]
hand.truth.df$hand.tree[image.num] <- p.tree
hand.truth.df$hand.soil[image.num] <- p.soil
hand.truth.df$hand.grass[image.num] <- p.grass
hand.truth.df$truth.maj.cover[image.num] <- maj.cov
hand.truth.df$tree_equation[image.num] <- tree.equation
hand.truth.df$grass_equation[image.num] <- grass.equation
hand.truth.df$soil_equation[image.num] <- soil.equation
hand.truth.df$tree.bright.thresh[image.num] <- tree.bright.thresh
hand.truth.df$grass.bright.thresh[image.num] <- grass.bright.thresh
hand.truth.df$grass.grvd.thresh[image.num] <- grass.grvd.thresh
hand.truth.df$bright.frac[image.num] <- bright.frac
hand.truth.df$red.frac[image.num] <- red.frac



#############################  Save DF  ##########################################################################
if(save.final.class.df){
  write.csv(hand.truth.df, paste0("manual_classification_table_", YMD(), ".csv"))
}











#### THIS was specific to Mpala. Was moving over old imagery that had been collected from Google so that wouldn't have to do again
# # Move the images over to new folder from the old 2017 folder
# old.folder.path <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/SMA/Rasters/test_images_20170102"
# image.list <- list.files(old.folder.path, full.names = T)
# 
# # Only take the big ones
# final.list <- c() # this will be filled
# for(i in 1:length(image.list)){
#   size <- file.size(image.list[i])
#   if(size > 350000){
#     final.list <- c(final.list, image.list[i])
#   }
# }
# 
# for(t in 1:500){
#   image.name <- final.list[t]
#   image.name.new <- paste0("./", ground.truth.image.folder, "/pixel_park", park.cell.nums[t, 1], 
#                            "_sample", park.cell.nums[t, 2], "_", park, ".png")
#   file.copy(image.name, image.name.new)
# }

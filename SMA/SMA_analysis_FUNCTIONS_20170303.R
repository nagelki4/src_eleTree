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
source("C:/Users/nagelki-4/Dropbox/Permanent/Grad School/Code/R/Function Scripts/SMA_Eval_Functions.R")

#################################  VARIABLES  ##################################################################
# Define variables
sample.size <- 500 # how many points should go into the ground truth? 
water.frac.thresh <- 0.05 # When water occupies what fraction of the scene will it be included in the classification? This was done because trees are sometimes classified as water, so only class water when it's fairly certain it's in the image. 
get.Google.image <- F  # Should the Google image be downloaded? (Set to FALSE if they're already downloaded)
Google.image.dim.final <- 30 # in meters, the resolution of the final Google image (after clipping) (This can be made into list of the different resolutions to analyze at. So could do accuracy assessment for 30 meters and then 120 as well, to see how compare)
save.rgb.pixel <- F  # Should the clipped Google image be saved?
save.class.pixel <- FALSE  # Save the classified Google image?
classify.water <- FALSE  # Should water be included as a condidate class?
save.confusion.matrix <- FALSE  # Save the confusion matrix?
save.transition.matrix <- FALSE  # Save the transition matrix?
check.ground.truth <- FALSE  # If true, this will run the ground truth hand classification code. Should always be FALSE
classify.google.images <- FALSE # should the Google images be classified? Unnecessary if already done and in memory
ndvi_n_msavi <- FALSE # calc NDVI and MSAVI2?
vcf_landsat <- FALSE  # load the 2015 VCF?
three_by_three <- TRUE # should the area be 3x3 landsat pixels for classification?
one_by_one <- FALSE  # sets classified area to a single cell of size Google.image.dim.final (30m)
plot.9by9 <- FALSE  # Should an image with the 9x9 cells be created for each rep?
is.hdr <- FALSE


# Landsat Folder
landsat.folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/SMA/Landsat/Mpala/"

# Location and prefixes of the 2014 and 1987 imagery
image_prefix <- "LC81680602014034LGN00"
image_folder <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/SMA/Landsat/Mpala/LC81680602014034-SC20160914165712/Original"
image87_prefix <- 
image87_folder <- 


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

###################################################################################################
###### NDVI and MSAVI2
if(ndvi_n_msavi){
  
  MSAVI2 <- MSAVI2_Landsat_calc(path = image_folder, prefix = image_prefix)
  NDVI <- NDVI_Landsat_calc(path = image_folder, prefix = image_prefix)
  
  # Write rasters to file
  writeRaster(NDVI, "NDVI", format = "GTiff")
  writeRaster(MSAVI2, "MSAVI2", format = "GTiff")
  
}


#############################  SET WORKING DIRECTORY  ######################################################################
# Set working directory to where the imagery should be saved
setwd(working.dir)
# par(mfrow = c(1,1)) # just in case needed for changing later
par(mar=c(1,1,2,1))



#####################  SHAPEFILE & RASTERS & POINT GENERATION  ############################################

###### Read in the shapefile for generating points within
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

###### LANDSAT #######
# Plot the RGBs from 1987 and 2014
stack.2014 <- stackLandsat(path = image_folder, prefix = image_prefix)
cr.stack.2014 <- crop(stack.2014, extent(mpala.boundary.simple)) # take down to extent of park
crop.stack.2014 <- mask(cr.stack.2014, mpala.boundary.simple) # clip by park boundary
rgbLandsat(stackname = crop.stack.2014)


red87 <- raster(paste0(landsat.folder, "mpalared_87")) # this can now be rewritten to the above for 87
green87 <- raster(paste0(landsat.folder, "mpalagreen_87"))
blue87 <- raster(paste0(landsat.folder, "mpalablue_87"))
stack.87 <- stack(red87, green87, blue87)
plotRGB(stack.87, scale = 255, axes = TRUE, main = "1987")



###### SMA & VCF #############################
# Read in rasters
mpala_cfmask <- raster("mpala_cfmask")
plot(mpala_cfmask, axes=F,box = F, legend = FALSE) # 0 is cloud free, 1 == cloud


## Read in and clip the Laikipia files for each year
Laik.14.tr <- raster(paste0(SMA.folder, SMA.14), band = tr.bandnum.14)
Laik.14.so <- raster(paste0(SMA.folder, SMA.14), band = gr.bandnum.14)
Laik.14.gr <- raster(paste0(SMA.folder, SMA.14), band = so.bandnum.14)
Laik.87.tr <- raster(paste0(SMA.folder, SMA.87), band = tr.bandnum.87) 
Laik.87.gr <- raster(paste0(SMA.folder, SMA.87), band = gr.bandnum.87)
Laik.87.so <- raster(paste0(SMA.folder, SMA.87), band = so.bandnum.87)



# Crop them all down to the Mpala extent
laik.14.tr <- crop(Laik.14.tr, extent(mpala.boundary.simple))
laik.14.gr <- crop(Laik.14.gr, extent(mpala.boundary.simple))
laik.14.so <- crop(Laik.14.so, extent(mpala.boundary.simple))
laik.87.tr <- crop(Laik.87.tr, extent(mpala.boundary.simple))
laik.87.gr <- crop(Laik.87.gr, extent(mpala.boundary.simple))
laik.87.so <- crop(Laik.87.so, extent(mpala.boundary.simple))


# Clip to the Mpala boundary
# 2014 
TREE <- mask(laik.14.tr, mpala.boundary.simple)
GRASS <- mask(laik.14.gr, mpala.boundary.simple)
SOIL <- mask(laik.14.so, mpala.boundary.simple)
# 1987
TREE.87 <- mask(laik.87.tr, mpala.boundary.simple)
GRASS.87 <- mask(laik.87.gr, mpala.boundary.simple)
SOIL.87 <- mask(laik.87.so, mpala.boundary.simple)


if(is.hdr){
  tr.max <- max(TREE[!is.na(TREE)])
  tr.min <- min(TREE[!is.na(TREE)])
  TREE[TREE > 0] <- TREE[TREE > 0]/tr.max*255
  TREE[TREE < 0] <- TREE[TREE < 0]/tr.min*255
  # TREE <- (TREE + abs(tr.min))/(tr.max - tr.min)*255
  tr.max <- max(GRASS[!is.na(GRASS)])
  tr.min <- min(GRASS[!is.na(GRASS)])
  GRASS[GRASS > 0] <- GRASS[GRASS > 0]/tr.max*255
  GRASS[GRASS < 0] <- GRASS[GRASS < 0]/tr.min*255
  # GRASS <- (GRASS + abs(tr.min))/(tr.max - tr.min)*255# Might want to do the same with negatives if this ends up overestimating tree cover
  tr.max <- max(SOIL[!is.na(SOIL)])
  tr.min <- min(SOIL[!is.na(SOIL)])
  SOIL[SOIL > 0] <- SOIL[SOIL > 0]/tr.max*255
  SOIL[SOIL < 0] <- SOIL[SOIL < 0]/tr.min*255
  # SOIL <- (SOIL + abs(tr.min))/(tr.max - tr.min)*255
}

plot(TREE, axes=F,box = F, main = "2014 Trees SMA")
plot(GRASS, axes=F,box = F, main = "2014 Grass SMA")
plot(SOIL, axes=F,box = F, main = "2014 Soil SMA")
plot(TREE.87, axes=F,box = F, main = "1987 Trees SMA")
plot(GRASS.87, axes=F,box = F, main = "1987 Grass SMA")
plot(SOIL.87, axes=F,box = F, main = "1987 Soil SMA")


# Stack to see if they are the same
cover.stack.14 <- stack(TREE, GRASS, SOIL)
cover.stack.87 <- stack(TREE.87, GRASS.87, SOIL.87)
par(mar=c(1,1,2,1))
plotRGB(cover.stack.14, scale = 255, axes = TRUE, main = "2014 SMA")
plotRGB(cover.stack.87, scale = 255, axes = TRUE, main = "1987 SMA")

###### POINT GENERATION ###############################
# Crop the area that will be used for random point generation. This could be done several
# times if using more polygons in the future.

# Clip out the raster and set the values to 1
plot(TREE, axes=F,box = F)
plot(ground.truth.frame, axes=F,box = F, add = TRUE)
sample.raster <- mask(TREE, ground.truth.frame) 
sample.raster[!is.na(sample.raster)] <- 1
plot(sample.raster, axes=F,box = F)

# Clip all the other rasters (this doesn't need to be done if you are sampling the entire area and can just be commented out if so)
# This is done so all the pixel numbers are the same, which is needed for the point sampling to work
TREE <- mask(TREE, ground.truth.frame)
GRASS <- mask(GRASS, ground.truth.frame)
SOIL <- mask(SOIL, ground.truth.frame)


if(ndvi_n_msavi){
  ndvi <- crop(NDVI, extent(mpala.boundary.simple))
  msavi <- crop(MSAVI2, extent(mpala.boundary.simple))
  ndvi <- mask(ndvi, ground.truth.frame)
  msavi <- mask(msavi, ground.truth.frame)
}

if(vcf_landsat){
  vcf.file <- raster(vcf)
  vcf.file <- crop(vcf.file, extent(mpala.boundary.simple))
  vcf.tree <- mask(vcf.file, ground.truth.frame)
}




######  GENERATE LIST OF POINTS  ########
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



###########  CLASSIFYING GOOGLE PIXELS  #########################################################

######## 1x1 Classification #######
if(one_by_one){
  ### Loop through each cell/pixel and classify and plug into table
  # First, make the table that values will be plugged into
  mx <- matrix(0, nrow = sample.size, ncol = 12)
  c.names <- c("Count", "pxl.num", "p.tree", "p.grass", "p.soil", "p.water", "maj.truth", "SMA.tree", "SMA.grass", "SMA.soil", "SMA.water", "maj.SMA")
  colnames(mx) <- c.names
  master.df <- as.data.frame(mx)
  
  ##  Loop through each pixel and classify and record values
  # t <- 1
  for(t in 1:sample.size){
    
    image.name <- paste0("./", google.image.folder, "/pixel_", cell.numbers[t], ".png")
    # Create the google image folder if not already done
    if(file.exists(paste0("./", google.image.folder)) == FALSE){
      dir.create(paste0("./", google.image.folder))
    }
    
    # GET THE IMAGE FROM GOOGLE!! (If not already done)
    if(get.Google.image){
      # Extract that pixel
      pxl <- rasterFromCells(sample.raster, cell.numbers[t])
      # plot(pxl)
      
      ### Get the corner coordinates
      # List the 2 opposite corners
      c1 <- cbind(xmin(pxl), ymin(pxl))
      c2 <- cbind(xmax(pxl), ymax(pxl))
      cc <- rbind(c1, c2)
      # Find projection and make spatial points
      prj <- crs(pxl)
      utmcoor2<-SpatialPoints(cc, proj4string=prj)
      
      # Convert to lat long
      latlong <- spTransform(utmcoor2, CRS("+proj=longlat"))
      # Make extent objects for plugging into google map function
      ext2 <- extent(latlong)
      x.range <- c(ext2@xmin, ext2@xmax)
      y.range <- c(ext2@ymin, ext2@ymax)
      
      # DOWNLOAD THE IMAGE
      j <- GetMap.bbox(x.range, y.range, destfile = image.name, maptype = "satellite", zoom = 19)
    }
    
    # if(classify.google.images){
    #########  CLIP GOOGLE IMAGE  ########
    # Now bring in the png that was just created
    image <- png.read.plot(image.name)
    
    #
    ### CLASSIFY - This could be written into its own for loop, along with the classification below this (2 loops)
    # Pull out the red green and blue bands
    red <- raster(image[,,1])
    green <- raster(image[,,2])
    blue <- raster(image[,,3])
    
    # Calculate how much to take off each end. The map is ~ 190x190 m
    image.indent <- (190 - Google.image.dim.final)/2
    # Crop the rasters
    one.side <- image.indent/190
    # Create the extent of the 30x30 pixel (the dimension can be changed in the variable section)
    cr.ext <- extent(c(one.side,1-one.side,one.side,1-one.side))
    
    # Crop
    red.c <- crop(red, cr.ext)
    green.c <- crop(green, cr.ext)
    blue.c <- crop(blue, cr.ext)
    
    # Plot and save the RGB image of the cell # not needed if they are already saved
    if(save.rgb.pixel){
      col.stack <- stack(red.c, green.c, blue.c)
      rgb.3030.name <- paste0("./", google.image.folder, "/pixel_", cell.numbers[t], "_3030_rgb.png")
      # Save the image
      png(rgb.3030.name)
      plotRGB(col.stack, scale = 1)
      dev.off()
    }
    
    
    ########  CLASSIFY  ###########
    # Sum the RGB values. Low values are dark areas which correspond to trees. 
    sum.im.pre <- green.c + red.c + blue.c
    
    # Calculate the Green-Red Vegetation Index
    grvd.pre <- (green.c - red.c) / (green.c + red.c)
    
    # Calculate summary stats for images to determine if it is mostly grass or soil
    #  This is done because images with a lot of grass or soil require different thresholds for classifying tree cover
    
    # Look at the brightness of each cell. Maybe that will be the best threshold for grass vs soil dominance
    bright <- red + green + blue
    bright.area <- bright > 2
    bright.frac <- length(bright.area[bright.area == 1]) / length(bright.area)
    
    
    # Look at pixels with higher red values than green
    red.area <- red > green
    red.frac <- length(red.area[red.area == 1]) / length(red.area) # 0.4391536
    
    # If including water, classify it first
    if(classify.water){
      # Classify water and include in classification if it takes up more than 5% of the image
      water <- sum.im.pre < 1.3 & grvd.pre < .03
      # plot(water, main = "Water")
      water.frac <- length(water[water == 1])/length(water[]) # calculate the fraction of water
      if(water.frac > water.frac.thresh){
        water.mask <- water
        water.mask[water.mask == 1] <- 50
        holy.image.1 <- grvd.pre + water.mask
        holy.image.2 <- sum.im.pre + water.mask
        holy.image.1[holy.image.1 > 5] <- NA
        holy.image.2[holy.image.2 > 5] <- NA
        grvd.pre <- holy.image.1
        sum.im.pre <- holy.image.2
      }
    }
    
    
    # TREES    
    # This tests for areas with more red and brighter to help with classification bc trees under repped in soil dominated
    # and over repped in grass/tree dominated images
    if(red.frac >= .4 & bright.frac >= .5 ){
      trees <- sum.im.pre < 1.5
      tree.equation <- "sum.im.pre < 1.5"
    }else if(red.frac <= .01 & bright.frac <= .05){
      trees <- sum.im.pre < 1.1
      tree.equation <- "sum.im.pre < 1.1"
    }else{
      trees <- sum.im.pre < 1.3
      tree.equation <- "sum.im.pre < 1.3"
    }
    
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
    grass <-  sum.im < 2.1 & grvd > -0.05
    grass.equation <- "sum.im < 2.1 & grvd > -0.05"
    
    # SOIL 
    # Soil is the rest
    soil <- grass == 0
    soil.equation <- "remainder"
    
    # Combine into one
    # Give the different cover types different values
    grass.ad <- grass
    grass.ad[grass.ad == 1] <- 2
    grass.ad[is.na(grass.ad)] <- 0
    
    soil.ad <- soil # don't need to set soil to a different value. Just let it be 1
    soil.ad[is.na(soil.ad)] <- 0
    
    tree.ad <- trees
    tree.ad[tree.ad == 1] <- 3
    tree.ad[is.na(tree.ad)] <- 0
    
    par(mar=c(1,1,3,1))
    
    if(classify.water){
      if(water.frac > water.frac.thresh){
        water.ad <- water
        water.ad[water.ad == 1] <- 4
        water.ad[is.na(water.ad)] <- 0
        tot.img <- tree.ad + soil.ad + grass.ad + water.ad
        tot.img[tot.img == 0] <- NA
        # plot(tot.img, axes=F,box = F, main = "4 = Water, 3 = Trees, 2 = Grass, 1 = Soil")
      } 
    } else{
      # Combine
      tot.img <- tree.ad + soil.ad + grass.ad
      tot.img[tot.img == 0] <- NA
      # plot(tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil")
    }
    
    
    # Plot and save final classification (if wanted)
    if(save.class.pixel){
      class.3030.name <- paste0("./", google.image.folder, "/pixel_", cell.numbers[t], "_3030_class_new_redbrightthresh.png")
      # Save the image
      png(class.3030.name)
      plot(tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil")
      dev.off()
    }
    # }
    #   if(!classify.google.images){ # the code could just read the master.df in and then fill in the SMA results
    #     # read in the image
    #     class.3030.name <- paste0("./", google.image.folder, "/pixel_", cell.numbers[t], "_3030_class_new_redbrightthresh.png")
    #     tot.img <- png.read.plot(class.3030.name)
    #     red <- raster(image[,,1])
    #     green <- raster(image[,,2])
    #     blue <- raster(image[,,3])
    #   }
    
    #############  RECORD PERCENT COVERS  ######################################################
    # Calculate the % covers
    # Get the number of cells in each
    wtr.cells <- length(tot.img[tot.img == 4]) 
    tr.cells <- length(tot.img[tot.img == 3])
    gr.cells <- length(tot.img[tot.img == 2])
    sl.cells <- length(tot.img[tot.img == 1])
    tot.cells <- ncell(tot.img)
    
    # Some cells are now NA because the classification excluded cover chances in some places, so switch to 0 
    if(is.na(TREE[cell.numbers[t]])){
      TREE[cell.numbers[t]] <- 0
    }
    
    if(is.na(GRASS[cell.numbers[t]])){
      GRASS[cell.numbers[t]] <- 0
    }
    
    if(is.na(SOIL[cell.numbers[t]])){
      SOIL[cell.numbers[t]] <- 0
    }
    
    # Compute percentages and what the majority cover is
    p.tree <- tr.cells / tot.cells
    p.grass <- gr.cells / tot.cells
    p.soil <- sl.cells / tot.cells
    p.water <- wtr.cells / tot.cells
    sma.tree <- TREE[cell.numbers[t]]/255  
    sma.grass <- GRASS[cell.numbers[t]]/255
    sma.soil <- SOIL[cell.numbers[t]]/255
    # Define majority cover for Google classification
    if(p.water > p.grass & p.water > p.soil & p.water > p.tree){
      maj.cov <- "Water"
    } else if(p.tree > p.grass & p.tree > p.soil & p.tree > p.water){
      maj.cov <- "Trees"
    } else if(p.grass > p.tree & p.grass > p.soil & p.grass > p.water){
      maj.cov <- "Grass"
    } else{maj.cov <- "Soil"}
    
    # Define major cover for SMA
    if(sma.tree > sma.grass & sma.tree > sma.soil){
      SMA.cov <- "Trees"
    } else if(sma.grass > sma.tree & sma.grass > sma.soil){
      SMA.cov <- "Grass"
    } else{SMA.cov <- "Soil"}
    
    # Plug them in
    master.df$Count[t] <- t
    master.df$pxl.num[t] <- cell.numbers[t]
    master.df$p.water[t] <- p.water
    master.df$p.tree[t] <- p.tree
    master.df$p.soil[t] <- p.soil
    master.df$p.grass[t] <- p.grass
    master.df$maj.truth[t] <- maj.cov
    master.df$SMA.tree[t] <- sma.tree
    master.df$SMA.grass[t] <- sma.grass
    master.df$SMA.soil[t] <- sma.soil
    master.df$maj.SMA[t] <- SMA.cov
    master.df$tree_equation[t] <- tree.equation
    master.df$grass_equation[t] <- grass.equation
    master.df$soil_equation[t] <- soil.equation
    if(ndvi_n_msavi){
      master.df$NDVI[t] <- ndvi[cell.numbers[t]]
      master.df$MSAVI2[t] <- msavi[cell.numbers[t]]
    }
  }
}



######## 3x3 Classification #######

# for the landsat cells, get the row and column of the sample point and define the 8 around it as 1 away from that point
# for the google cells, will have to construct the different indents for each surrounding 30m box




if(three_by_three){
  # First, make the table that values will be plugged into
  mx <- matrix(0, nrow = sample.size, ncol = 12)
  c.names <- c("Count", "pxl.num", "p.tree", "p.grass", "p.soil", "p.water", "maj.truth", "SMA.tree", "SMA.grass", "SMA.soil", "SMA.water", "maj.SMA")
  colnames(mx) <- c.names
  master.df <- as.data.frame(mx)
  
  # Make the 3x3 table
  mx3 <- matrix(0, nrow = sample.size*9, ncol = 6)
  c3.names <- c("Count", "pxl.num", "number.of.9", "tree", "grass", "soil")
  colnames(mx3) <- c3.names
  nine.pix.df <- as.data.frame(mx3)
  
  # Start a 9 pixel count (will total the sample size * 9 in the end)
  count.nine.pix <- 1
  
  # Index list for the extents. Used to count through the 9 cells
  extent.list <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
  
  # Generate stats for indenting Google image to get the 9 different cells within it
  cell.size <- Google.image.dim.final/190 # 190m is the length of one side of the Google image
  half.pix <- cell.size / 2
  center <- .5 # Middle of the image. The reference point for indexing
  
  # Create the boundaries of the upper left cell. The loop will modify these as it counts through the cells (extent.list)
  bottom <- center + half.pix
  right <- center - half.pix
  top <- center + half.pix + cell.size 
  left <- center - half.pix - cell.size # might be able to simplify these. Write whole loop first
  
  ##  Loop through each pixel and classify and record values
  # t <- 1
  for(t in 1:sample.size){
    
    image.name <- paste0("./", google.image.folder, "/pixel_", cell.numbers[t], ".png")
    # Create the google image folder if not already done
    if(file.exists(paste0("./", google.image.folder)) == FALSE){
      dir.create(paste0("./", google.image.folder))
    }
    
    # GET THE IMAGE FROM GOOGLE!! (If not already done)
    if(get.Google.image){
      # Extract that pixel
      pxl <- rasterFromCells(sample.raster, cell.numbers[t])
      # plot(pxl)
      
      ### Get the corner coordinates
      # List the 2 opposite corners
      c1 <- cbind(xmin(pxl), ymin(pxl))
      c2 <- cbind(xmax(pxl), ymax(pxl))
      cc <- rbind(c1, c2)
      # Find projection and make spatial points
      prj <- crs(pxl)
      utmcoor2<-SpatialPoints(cc, proj4string=prj)
      
      # Convert to lat long
      latlong <- spTransform(utmcoor2, CRS("+proj=longlat"))
      # Make extent objects for plugging into google map function
      ext2 <- extent(latlong)
      x.range <- c(ext2@xmin, ext2@xmax)
      y.range <- c(ext2@ymin, ext2@ymax)
      
      # DOWNLOAD THE IMAGE
      j <- GetMap.bbox(x.range, y.range, destfile = image.name, maptype = "satellite", zoom = 19)
    }
    
    # if(classify.google.images){
    #########  CLIP GOOGLE IMAGE  ########
    # Now bring in the png that was just created
    image <- png.read.plot(image.name)
    
    #
    ### CLASSIFY - This could be written into its own for loop, along with the classification below this (2 loops)
    # Pull out the red green and blue bands
    red <- raster(image[,,1])
    green <- raster(image[,,2])
    blue <- raster(image[,,3])
    
    # Calculate how much to take off each end. The map is ~ 190x190 m
    image.indent <- (190 - Google.image.dim.final)/2
    # Crop the rasters
    one.side <- image.indent/190
    # Create the extent of the 30x30 pixel (the dimension can be changed in the variable section)
    cr.ext <- extent(c(one.side,1-one.side,one.side,1-one.side))
    
    # Crop
    red.c <- crop(red, cr.ext)
    green.c <- crop(green, cr.ext)
    blue.c <- crop(blue, cr.ext)
    
    
    ########  CLASSIFY  ###########
    # Sum the RGB values. Low values are dark areas which correspond to trees. 
    sum.im.pre <- green.c + red.c + blue.c
    
    # Calculate the Green-Red Vegetation Index
    grvd.pre <- (green.c - red.c) / (green.c + red.c)
    
    # Calculate summary stats for images that will be used to determine if it is mostly grass or soil
    #  This is done because images with a lot of grass or soil require different thresholds for classifying tree cover
    # What proportion of cells have a total brightness over 2?
    bright <- red + green + blue
    bright.area <- bright > 2
    bright.frac <- length(bright.area[bright.area == 1]) / length(bright.area)
    
    
    # Look at proportion of pixels with higher red values than green. This is another for determining if mostly veg or soil
    red.area <- red > green
    red.frac <- length(red.area[red.area == 1]) / length(red.area) # 0.4391536
    
    # Water classification code is in the 1x1 code
    
    
    # TREES    
    # This tests for areas with more red and brighter to help with classification bc trees under repped in soil dominated
    # and over repped in grass/tree dominated images
    if(red.frac >= .4 & bright.frac >= .5 ){
      trees <- sum.im.pre < 1.5
      tree.equation <- "sum.im.pre < 1.5"
    }else if(red.frac <= .01 & bright.frac <= .05){
      trees <- sum.im.pre < 1.1
      tree.equation <- "sum.im.pre < 1.1"
    }else{
      trees <- sum.im.pre < 1.3
      tree.equation <- "sum.im.pre < 1.3"
    }
    
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
    grass <-  sum.im < 2.1 & grvd > -0.05
    grass.equation <- "sum.im < 2.1 & grvd > -0.05"
    
    # SOIL 
    # Soil is the rest
    soil <- grass == 0
    soil.equation <- "remainder"
    
    # Combine into one
    # Give the different cover types different values
    grass.ad <- grass
    grass.ad[grass.ad == 1] <- 2
    grass.ad[is.na(grass.ad)] <- 0
    
    soil.ad <- soil # don't need to set soil to a different value. Just let it be 1
    soil.ad[is.na(soil.ad)] <- 0
    
    tree.ad <- trees
    tree.ad[tree.ad == 1] <- 3
    tree.ad[is.na(tree.ad)] <- 0
    
    par(mar=c(1,1,3,1))
    
    # Combine
    tot.img <- tree.ad + soil.ad + grass.ad
    tot.img[tot.img == 0] <- NA
    # plot(tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil")
    
    # lines to save the classed image are in 1x1 code (above)
    
    #############  RECORD PERCENT COVERS  ######################################################
    # Calculate the % covers
    # Get the number of cells in each
    wtr.cells <- length(tot.img[tot.img == 4]) 
    tr.cells <- length(tot.img[tot.img == 3])
    gr.cells <- length(tot.img[tot.img == 2])
    sl.cells <- length(tot.img[tot.img == 1])
    tot.cells <- ncell(tot.img)
    
    # Some cells are now NA because the classification excluded cover chances in some places, so switch to 0. This shouldn't be an issue anymore
    if(is.na(TREE[cell.numbers[t]])){
      TREE[cell.numbers[t]] <- 0
    }
    
    if(is.na(GRASS[cell.numbers[t]])){
      GRASS[cell.numbers[t]] <- 0
    }
    
    if(is.na(SOIL[cell.numbers[t]])){
      SOIL[cell.numbers[t]] <- 0
    }
    
    # Compute percentages and what the majority cover is
    p.tree <- tr.cells / tot.cells
    p.grass <- gr.cells / tot.cells
    p.soil <- sl.cells / tot.cells
    p.water <- wtr.cells / tot.cells
    sma.tree <- TREE[cell.numbers[t]]/255  
    sma.grass <- GRASS[cell.numbers[t]]/255
    sma.soil <- SOIL[cell.numbers[t]]/255
    # Define majority cover for Google classification
    if(p.water > p.grass & p.water > p.soil & p.water > p.tree){
      maj.cov <- "Water"
    } else if(p.tree > p.grass & p.tree > p.soil & p.tree > p.water){
      maj.cov <- "Trees"
    } else if(p.grass > p.tree & p.grass > p.soil & p.grass > p.water){
      maj.cov <- "Grass"
    } else{maj.cov <- "Soil"}
    
    # Define major cover for SMA
    if(sma.tree > sma.grass & sma.tree > sma.soil){
      SMA.cov <- "Trees"
    } else if(sma.grass > sma.tree & sma.grass > sma.soil){
      SMA.cov <- "Grass"
    } else{SMA.cov <- "Soil"}
    
    # Plug them in
    master.df$Count[t] <- t
    master.df$pxl.num[t] <- cell.numbers[t]
    master.df$p.water[t] <- p.water
    master.df$p.tree[t] <- p.tree
    master.df$p.soil[t] <- p.soil  # make a seperate table for the 1-9 values. Only record the average here
    master.df$p.grass[t] <- p.grass 
    master.df$maj.truth[t] <- maj.cov 
    master.df$SMA.tree[t] <- sma.tree 
    master.df$SMA.grass[t] <- sma.grass 
    master.df$SMA.soil[t] <- sma.soil
    master.df$maj.SMA[t] <- SMA.cov
    master.df$tree_equation[t] <- tree.equation
    master.df$grass_equation[t] <- grass.equation
    master.df$soil_equation[t] <- soil.equation
    if(ndvi_n_msavi){
      master.df$NDVI[t] <- ndvi[cell.numbers[t]]
      master.df$MSAVI2[t] <- msavi[cell.numbers[t]]
    }
    master.df$VCF.tree[t] <- vcf.tree[cell.numbers[t]]/100
    
    
    ############## Create the 3 x 3 table ########
    
    # Create vectors to fill in the 9 values
    nine.cell.vector.tree <- c()
    nine.cell.vector.grass <- c()
    nine.cell.vector.soil <- c()
    
    # r <- 0
    for(r in extent.list){ # This loop starts with the upper right of the nine cells, and reads down the first column, then second, then third
      # CREATE THE EXTENT
      # Calculate the extent of each cell. The map is ~ 190x190 m, ext(map) = 0, 1, 0, 1
      
      # First column
      if(r <= 2){
        bt <- bottom - cell.size*r
        rt <- right 
        tp <- top - cell.size*r
        lf <- left
      }else if(r <= 5){ # Second column
        bt <- bottom - cell.size*(r-3)
        rt <- right + cell.size 
        tp <- top - cell.size*(r-3)
        lf <- left + cell.size
      }else if(r <= 8){ # Third
        bt <- bottom - cell.size*(r-6)
        rt <- right + cell.size*2 
        tp <- top - cell.size*(r-6)
        lf <- left + cell.size*2
      }
      
      # Create extent object 
      cr.ext <- extent(c(lf, rt, bt, tp))
      
      if(plot.9by9){# Plot one of the layers to look at where the cells end up. Should be 3x3 grid
      plot(red)
      # Coerce extent to a SpatialPolygons object and add to plot
      cell.poly <- as(cr.ext, 'SpatialPolygons')
      plot(cell.poly, add = TRUE)
      }
      
      
      # Crop
      red.c <- crop(red, cr.ext)
      green.c <- crop(green, cr.ext)
      blue.c <- crop(blue, cr.ext)
      
      
      ########  CLASSIFY  ###########
      # Sum the RGB values. Low values are dark areas which correspond to trees. 
      sum.im.pre <- green.c + red.c + blue.c
      
      # Calculate the Green-Red Vegetation Index
      grvd.pre <- (green.c - red.c) / (green.c + red.c)
      
      # Calculate summary stats for images that will be used to determine if it is mostly grass or soil
      #  This is done because images with a lot of grass or soil require different thresholds for classifying tree cover
      # What proportion of cells have a total brightness over 2?
      bright <- red + green + blue
      bright.area <- bright > 2
      bright.frac <- length(bright.area[bright.area == 1]) / length(bright.area)
      
      
      # Look at proportion of pixels with higher red values than green. This is another for determining if mostly veg or soil
      red.area <- red > green
      red.frac <- length(red.area[red.area == 1]) / length(red.area) # 0.4391536
      
      # Water classification code is in the 1x1 code
      
      
      # TREES    
      # This tests for areas with more red and brighter to help with classification bc trees under repped in soil dominated
      # and over repped in grass/tree dominated images
      if(red.frac >= .4 & bright.frac >= .5 ){
        trees <- sum.im.pre < 1.5
        tree.equation <- "sum.im.pre < 1.5"
      }else if(red.frac <= .01 & bright.frac <= .05){
        trees <- sum.im.pre < 1.1
        tree.equation <- "sum.im.pre < 1.1"
      }else{
        trees <- sum.im.pre < 1.3
        tree.equation <- "sum.im.pre < 1.3"
      }
      
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
      grass <-  sum.im < 2.1 & grvd > -0.05
      grass.equation <- "sum.im < 2.1 & grvd > -0.05"
      
      # SOIL 
      # Soil is the rest
      soil <- grass == 0
      soil.equation <- "remainder"
      
      # Combine into one
      # Give the different cover types different values
      grass.ad <- grass
      grass.ad[grass.ad == 1] <- 2
      grass.ad[is.na(grass.ad)] <- 0
      
      soil.ad <- soil # don't need to set soil to a different value. Just let it be 1
      soil.ad[is.na(soil.ad)] <- 0
      
      tree.ad <- trees
      tree.ad[tree.ad == 1] <- 3
      tree.ad[is.na(tree.ad)] <- 0
      
      par(mar=c(1,1,3,1))
      
      # Combine
      tot.img <- tree.ad + soil.ad + grass.ad
      tot.img[tot.img == 0] <- NA
      # plot(tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil")
      
      # lines to save the classed image are in 1x1 code (above)
      
      #############  RECORD PERCENT COVERS  ######################################################
      # Calculate the % covers
      # Get the number of cells in each
      wtr.cells <- length(tot.img[tot.img == 4]) 
      tr.cells <- length(tot.img[tot.img == 3])
      gr.cells <- length(tot.img[tot.img == 2])
      sl.cells <- length(tot.img[tot.img == 1])
      tot.cells <- ncell(tot.img)
      
      p.tree <- tr.cells/tot.cells
      p.grass <- gr.cells/tot.cells
      p.soil <- sl.cells/tot.cells
      
      nine.cell.vector.tree <- c(nine.cell.vector.tree, p.tree)
      nine.cell.vector.grass <- c(nine.cell.vector.grass, p.grass)
      nine.cell.vector.soil <- c(nine.cell.vector.soil, p.soil)
      
      # Calc the cell number of the 9
      pix.num <- length(nine.cell.vector.tree)
      
      
      # Plug into df
      nine.pix.df$Count[count.nine.pix] <- count.nine.pix
      nine.pix.df$pxl.num[count.nine.pix] <- cell.numbers[t]
      nine.pix.df$number.of.9[count.nine.pix] <- pix.num
      nine.pix.df$tree[count.nine.pix] <- p.tree
      nine.pix.df$grass[count.nine.pix] <- p.grass
      nine.pix.df$soil[count.nine.pix] <- p.soil
      
      # Print
      print(count.nine.pix)
      
      # Add 1 to the count
      count.nine.pix <- count.nine.pix + 1
    }
    master.df$nine.tree[t] <- mean(nine.cell.vector.tree)
    master.df$nine.grass[t] <- mean(nine.cell.vector.grass)
    master.df$nine.soil[t] <- mean(nine.cell.vector.soil)
  }
}
write.csv(master.df, "master.df_20170116.csv")
write.csv(nine.pix.df, "nine.pix.df.csv")

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




if(ndvi_n_msavi){
#### Plot 1:1
# NDVI - TREE
# Calculate Stats
correl <- cor(master.df$p.tree, master.df$NDVI)
covar <- cov(master.df$p.tree, master.df$NDVI)
md <- lm(master.df$NDVI ~ master.df$p.tree)

# Plot
plot(master.df$p.tree, master.df$NDVI, main = paste0("Observed Tree Cover vs. NDVI \nCor = ", round(correl, 2)), xlab = "Observed Tree Cover Fraction", ylab = "NDVI", xlim = c(0,1), ylim = c(0,1))
abline(md) # Add the 1:1 line
summary(md)

# MSAVI2 - TREE
# Calculate Stats
correl <- cor(master.df$p.tree, master.df$MSAVI2)
covar <- cov(master.df$p.tree, master.df$MSAVI2)
md <- lm(master.df$MSAVI2 ~ master.df$p.tree)
# Plot
plot(master.df$p.tree, master.df$MSAVI2, main = paste0("Observed Tree Cover vs. MSAVI2 \nCor = ", round(correl, 2)), xlab = "Observed Tree Cover Fraction", ylab = "MSAVI2", xlim = c(0,1), ylim = c(0,1))
abline(md) # Add the 1:1 line
summary(md)



# NDVI - GRASS
# Calculate Stats
correl <- cor(master.df$p.grass, master.df$NDVI)
covar <- cov(master.df$p.grass, master.df$NDVI)
md <- lm(master.df$NDVI ~ master.df$p.grass)

# Plot
plot(master.df$p.grass, master.df$NDVI, main = paste0("Observed grass Cover vs. NDVI \nCor = ", round(correl, 2)), xlab = "Observed grass Cover Fraction", ylab = "NDVI", xlim = c(0,1), ylim = c(0,1))
abline(md) # Add the 1:1 line
summary(md)

# MSAVI2 - GRASS
# Calculate Stats
correl <- cor(master.df$p.grass, master.df$MSAVI2)
covar <- cov(master.df$p.grass, master.df$MSAVI2)
md <- lm(master.df$MSAVI2 ~ master.df$p.grass)
# Plot
plot(master.df$p.grass, master.df$MSAVI2, main = paste0("Observed grass Cover vs. MSAVI2 \nCor = ", round(correl, 2)), xlab = "Observed grass Cover Fraction", ylab = "MSAVI2", xlim = c(0,1), ylim = c(0,1))
abline(md) # Add the 1:1 line
summary(md)




# NDVI - Soil
# Calculate Stats
correl <- cor(master.df$p.soil, master.df$NDVI)
covar <- cov(master.df$p.soil, master.df$NDVI)
md <- lm(master.df$NDVI ~ master.df$p.soil)

# Plot
plot(master.df$p.soil, master.df$NDVI, main = paste0("Observed soil Cover vs. NDVI \nCor = ", round(correl, 2)), xlab = "Observed soil Cover Fraction", ylab = "NDVI", xlim = c(0,1), ylim = c(0,1))
abline(md) # Add the 1:1 line
summary(md)

# MSAVI2 - Soil
# Calculate Stats
correl <- cor(master.df$p.soil, master.df$MSAVI2)
covar <- cov(master.df$p.soil, master.df$MSAVI2)
md <- lm(master.df$MSAVI2 ~ master.df$p.soil)
# Plot
plot(master.df$p.soil, master.df$MSAVI2, main = paste0("Observed soil Cover vs. MSAVI2 \nCor = ", round(correl, 2)), xlab = "Observed soil Cover Fraction", ylab = "MSAVI2", xlim = c(0,1), ylim = c(0,1))
abline(md) # Add the 1:1 line
summary(md)

}
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










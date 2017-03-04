########## TESTING CLASSIFICATION ###########################################

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

#################################  Workspace  ##################################################################
# Load the old workspace
load("X:/nagelki4/Projects/EleTree/R Workspaces/.RData")

#############################  SET WORKING DIRECTORY  #########################################################
# Set working directory to where the imagery should be saved
setwd(working.dir)
# par(mfrow = c(1,1)) # just in case needed for changing later
par(mar=c(1,1,2,1))


  setwd("C:/Users/nagelki-4/Dropbox/Permanent/Grad School/Classes/RS Biosphere/Project/Images/Accuracy/Testphoto")
  
  image <- jpg.read.plot("Test_photo3.JPG")
  
  setwd("C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Classes/RS Biosphere/Project/SpectralUnmix/Rasters/test_images_20161204")
  image.num <- 129
  file.lst <- list.files( , pattern = "rgb")
  class.list <- list.files( , pattern = "class")
  image <- png.read.plot(file.lst[image.num])
  png.read.plot(class.list[image.num])
  
  which(file.lst == "pixel_206481_3030_rgb.png")
  
  # Below will be a for loop 
  # For each image, the indices and the percent tree, soil and grass will be calculated
  
  # Pull out the red green and blue bands
  red <- raster(image[,,1])
  green <- raster(image[,,2])
  blue <- raster(image[,,3])
  
  # Create the map. Classify trees first then remove them from the image and classify soils and grass from remaining pixels
  # The way I'm going to do this will always take away from soils when shadows fall on soil, bc the actual crown will be interpretted 
  # as grass. That means when the shadows fall on grass, they won't take away from grass. So this approach is biased toward lowering
  # the area soil occupies. 
  
  # Sum the RGB values. Low values are dark areas which correspond to trees. (Watch out for water!)
  sum.im.pre <- green + red + blue
  
  # Calculate the Green-Red Vegetation Index
  grvd.pre <- (green - red) / (green + red)
  
  # Classify water and include in classification if it takes up more than 5% of the image
  # plot(sum.im)
  # plot(grvd.pre)
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
  
  
  
  # Identify trees (This doesn't work well in areas with no grass and no shadow. Could include test for % soil cover)
  # plot(sum.im.pre < 1.3 & grvd.pre > -.1)
  trees <- sum.im.pre < 1.3 & grvd.pre > -.1
  # plot(trees, main = "Trees")
  
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
  
  # Split soil and grass using GRVD and brightness. Soil is nonveg and bright
  # plot(grvd)
  # plot(sum.im)
  # plot(sum.im < 2.0 & grvd > 0.04) 
  grass <- sum.im < 2.0 & grvd > 0.04
  # plot(grass, main = "Grass")
  
  # Soil is the rest
  soil <- grass == 0
  # plot(soil, main = "Soil")
  
  # Combine into one
  # Give the different cover types different values
  grass.ad <- grass
  grass.ad[grass.ad == 1] <- 2
  grass.ad[is.na(grass.ad)] <- 0
  
  soil.ad <- soil
  soil.ad[is.na(soil.ad)] <- 0
  
  tree.ad <- trees
  tree.ad[tree.ad == 1] <- 4
  tree.ad[is.na(tree.ad)] <- 0
  
  if(water.frac > water.frac.thresh){
    water.ad <- water
    water.ad[water.ad == 1] <- 3
    tot.img <- tree.ad + soil.ad + grass.ad + water.ad
    tot.img[tot.img == 0] <- NA
    plot(tot.img, axes=F,box = F, main = "4 = Trees, 3 = Water, 2 = Grass, 1 = Soil")
  } else{
    # Combine
    tot.img <- tree.ad + soil.ad + grass.ad
    tot.img[tot.img == 0] <- NA
    tot.img[tot.img == 4] <- 3
    plot(tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil")
  }





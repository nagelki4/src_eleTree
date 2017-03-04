######  CHECKING GROUND TRUTH BY HAND  #####################################################

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


#############################  HAND CHECK ACTIVE CODE  #######################################################


if(check.ground.truth){
  # Reassign master.df to a new table with only first 50 entries (needs to be ordered)
  hand.check <- master.df[with(master.df, order(pxl.num)), ] [1:50, 1:7]
  hand.check.tot.new <- hand.check.tot
  # Get list of the first 50 RGB google images
  rgb.list <- list.files(paste0("./", google.image.folder), pattern = "_3030_rgb.png", full.names = TRUE)[1:50]
  
  p <- 1
  for(p in 1:50){
    # Index value
    index <- p
    
    # Write a manual loop, with 1 being added to the index at the end. Just click through
    #  Read in image
    img <- png.read.plot(rgb.list[index])
    
    #  Confirm cell number and file name match by checking for the cell number in rgb name
    if(grep(hand.check.tot.new$pxl.num[index], rgb.list[index])){
      print("It matches")
    }
    
    # Pull out the red green and blue bands
    red <- raster(img[,,1])
    green <- raster(img[,,2])
    blue <- raster(img[,,3])
    
    red.area <- red > green
    red.frac <- length(red.area[red.area == 1]) / length(red.area) # 0.4391536
    
    #     # Plug into new column 
    #     hand.check.tot.new$red.frac.area[p] <- red.frac
    
    # This worked somewhat well, but it really isn't getting at the area that appears red and green. Should count number of cells that where red is higher than green and blue (soil) and where green is higher than red and blue (grass). Might just want to say the number of cells where red is less than green and blue, though, because a lot of veg looks blue.
    # Look at the fraction of cells where red is the dominant color band
    #     red.gb <- red > blue
    #     red.gg <- red > green
    #     rr <- red.gb + red.gg
    #     rf <- rr[rr == 2]
    #     red.dom <- length(rf)/length(rr[])
    #     
    #     hand.check.tot.new$red.dom[p] <- red.dom # Not much difference!
    
    # Look at the brightness of each cell. Maybe that will be the best threshold for grass vs soil dominance
    bright <- red + green + blue
    bright.area <- bright > 2
    bright.frac <- length(bright.area[bright.area == 1]) / length(bright.area)
    hand.check.tot.new$bright[p] <- bright.frac
    
    # Now try with the new bright threshold, the red, and both together and compare with plots
    # Sum the RGB values. Low values are dark areas which correspond to trees. 
    sum.im.pre <- green + red + blue
    
    # Calculate the Green-Red Vegetation Index
    grvd.pre <- (green - red) / (green + red)
    
    # TREES 
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
    par(mfrow = c(1,2))
    
    # Combine
    hand.tot.img <- tree.ad + soil.ad + grass.ad
    hand.tot.img[hand.tot.img == 0] <- NA
    
    
    
    
    #  Record in dataframe
    # Calculate the % covers
    # Get the number of cells in each
    tr.cells <- length(hand.tot.img[hand.tot.img == 3])
    gr.cells <- length(hand.tot.img[hand.tot.img == 2])
    sl.cells <- length(hand.tot.img[hand.tot.img == 1])
    tot.cells <- ncell(hand.tot.img)
    
    #  Record in dataframe
    # Calculate the % covers
    # Get the number of cells in each
    tr.cells <- length(hand.tot.img[hand.tot.img == 3])
    gr.cells <- length(hand.tot.img[hand.tot.img == 2])
    sl.cells <- length(hand.tot.img[hand.tot.img == 1])
    tot.cells <- ncell(hand.tot.img)
    
    hand.tree <- tr.cells / tot.cells
    hand.grass <- gr.cells / tot.cells
    hand.soil <- sl.cells / tot.cells
    
    # Determine major cover type
    if(hand.tree > hand.grass & hand.tree > hand.soil){
      hand.maj.cov <- "Trees"
    } else if(hand.grass > hand.tree & hand.grass > hand.soil){
      hand.maj.cov <- "Grass"
    } else{hand.maj.cov <- "Soil"}
    
    # Plug them in. Add columns for %'s t, g, s by hand, along with thresholds (could be text, like "grvi > 4")
    hand.check.tot.new$p.tree[index] <- hand.tree
    hand.check.tot.new$p.grass[index] <- hand.grass
    hand.check.tot.new$p.soil[index] <- hand.soil
    hand.check.tot.new$maj.truth[index] <- hand.maj.cov
    hand.check.tot.new$new.tree.equation[index] <- tree.equation
    hand.check.tot.new$new.grass.equation[index] <- grass.equation
    hand.check.tot.new$new.soil.equation[index] <- soil.equation
    hand.check.tot.new$vcf.tree[index] <- # still gotta get the cell numbers 
      
  }
  
  
  
  
  
  
  
  # Plot image
  img.stack <- stack(red, green, blue)
  plotRGB(img.stack, scale = 1)
  
  #  Confirm cell number and file name match by checking for the cell number in rgb name
  if(grep(hand.check$pxl.num[index], rgb.list[index])){
    print("It matches")
  }
  
  #  Classify
  par(mfrow = c(1,2))
  # Sum the RGB values. Low values are dark areas which correspond to trees. 
  sum.im.pre <- green + red + blue
  
  # Calculate the Green-Red Vegetation Index
  grvd.pre <- (green - red) / (green + red)
  
  # TREES    
  # plot(sum.im.pre < 1.3 & grvd.pre > -.1)
  trees <- sum.im.pre < 1.0
  tree.equation <- "sum.im.pre < 1.0"
  plot(trees, main = "Trees", legend = FALSE)
  plotRGB(img.stack, scale = 1)
  
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
  # grass <- sum.im < 2.0 & grvd > 0.04
  grass <-  sum.im < 2.0 & grvd > -0.04
  grass.equation <- "sum.im < 2.0 & grvd > -0.04"
  plot(grass, main = "Grass")
  plotRGB(img.stack, scale = 1)
  
  # SOIL 
  # Soil is the rest
  soil <- grass == 0
  soil.equation <- "remainder"
  plot(soil, main = "Soil")
  plotRGB(img.stack, scale = 1)
  
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
  par(mfrow = c(1,2))
  
  # Combine
  hand.tot.img <- tree.ad + soil.ad + grass.ad
  hand.tot.img[hand.tot.img == 0] <- NA
  plot(hand.tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil", legend = FALSE)
  plotRGB(img.stack, scale = 1)
  
  # Plot and save final classification (if wanted)
  hand.3030.name <- paste0("./", google.image.folder, "/pixel_", hand.check$pxl.num[index], "_3030_hand.png")
  # Save the image
  par(mfrow = c(1,1))
  png(hand.3030.name)
  plot(hand.tot.img, axes=F,box = F, main = "3 = Trees, 2 = Grass, 1 = Soil")
  dev.off()
  
  
  #  Record in dataframe
  # Calculate the % covers
  # Get the number of cells in each
  tr.cells <- length(hand.tot.img[hand.tot.img == 3])
  gr.cells <- length(hand.tot.img[hand.tot.img == 2])
  sl.cells <- length(hand.tot.img[hand.tot.img == 1])
  tot.cells <- ncell(hand.tot.img)
  
  hand.tree <- tr.cells / tot.cells
  hand.grass <- gr.cells / tot.cells
  hand.soil <- sl.cells / tot.cells
  
  # Determine major cover type
  if(hand.tree > hand.grass & hand.tree > hand.soil){
    hand.maj.cov <- "Trees"
  } else if(hand.grass > hand.tree & hand.grass > hand.soil){
    hand.maj.cov <- "Grass"
  } else{hand.maj.cov <- "Soil"}
  
  # Plug them in. Add columns for %'s t, g, s by hand, along with thresholds (could be text, like "grvi > 4")
  hand.check$hand.tree[index] <- hand.tree
  hand.check$hand.grass[index] <- hand.grass
  hand.check$hand.soil[index] <- hand.soil
  hand.check$hand.maj.cov[index] <- hand.maj.cov
  hand.check$tree.equation[index] <- tree.equation
  hand.check$grass.equation[index] <- grass.equation
  hand.check$soil.equation[index] <- soil.equation
  hand.check$water.pres[index] <- 0
  
  # Add 1 to the indexing value
  index <- index + 1
  write.csv(hand.check.tot, "hand.check.tot.csv")
  
  print(index)
  
  ####### Do 1:1 plot
  
  # Add the SMA results back
  x.add <- master.df[with(master.df, order(pxl.num)), ] [1:50, c(2, 8:12)]
  hand.check.tot <- cbind(hand.check, x.add[ , 2:6])
  # hand.check.tot <- hand.check.tot.new
  
  # Change the margins back to the default
  par(mar=c(5.1,4.1,4.1,2.1))
  par(mfrow = c(1,1))
  # What column numbers are the tree values?
  p.tr.col <- which(names(hand.check.tot) == "p.tree")
  SMA.tr.col <- which(names(hand.check.tot) == "SMA.tree")
  # Now can index along this in a loop
  col.number <- c(p.tr.col, SMA.tr.col)
  # For title indexing
  title.text <- c("Google", "SMA")
  
  # m <- 1 
  
  for(m in 1:2){
    # TREES
    # Calculate Stats
    ##   RMSE
    tree.rmse <- sqrt( mean( (hand.check.tot$hand.tree - hand.check.tot[ , col.number[m]])^2 , na.rm = TRUE ) )
    ##   R-squared
    y.mean <- mean(hand.check.tot[ , col.number[m]])
    y.line <- hand.check.tot$hand.tree
    y <- hand.check.tot[ , col.number[m]]
    SE.y.mean <- sum((y - y.mean)^2) # what is the standard error from the mean of y? (SMA tree cover)
    SE.y.line <- sum((y - y.line)^2) # what is the standard error from the 1:1 line? (Google tree cover)
    tree.r.sq <- 1 - SE.y.line/SE.y.mean # NOTE 1 (see bottom)
    correl <- cor(hand.check.tot$hand.tree, hand.check.tot[ , col.number[m]])
    covar <- cov(hand.check.tot$hand.tree, hand.check.tot[ , col.number[m]])
    # Plot
    plot(hand.check.tot$hand.tree*100, hand.check.tot[ , col.number[m]]*100, main = paste0("Percent Tree Cover: By Hand vs. ", title.text[m], "\nRMSE = ", round(tree.rmse, 2)*100, "%  R-squared = ", round(tree.r.sq, 2), " Cor = ", round(correl, 2)), xlab = "By Hand % Tree Cover", ylab = paste0(title.text[m], " % Tree Cover"), xlim = c(0,100), ylim = c(0,100))
    abline(0,1) # Add the 1:1 line
    
    
    # GRASS
    # Calculate Stats
    ## RMSE
    grass.rmse <- sqrt( mean( (hand.check.tot$hand.grass- hand.check.tot[ , col.number[m] + 1])^2 , na.rm = TRUE ) )
    ## R-squared
    y.mean <- mean(hand.check.tot[ , col.number[m] + 1])
    y.line <- hand.check.tot$hand.grass
    y <- hand.check.tot[ , col.number[m] + 1]
    SE.y.mean <- sum((y - y.mean)^2) # what is the standard error from the mean of y? (SMA grass cover)
    SE.y.line <- sum((y - y.line)^2) # what is the standard error from the 1:1 line? (Google grass cover)
    grass.r.sq <- 1 - SE.y.line/SE.y.mean
    correl <- cor(hand.check.tot$hand.tree, hand.check.tot[ , col.number[m] + 1])
    covar <- cov(hand.check.tot$hand.tree, hand.check.tot[ , col.number[m] + 1])
    # Plot
    plot(hand.check.tot$hand.grass*100, hand.check.tot[ , col.number[m] + 1]*100, main = paste0("Percent Grass Cover: By Hand vs. ", title.text[m], "\nRMSE = ", round(grass.rmse, 2)*100, "%  R-squared = ", round(grass.r.sq, 2), " Cor = ", round(correl, 2)), xlab = "By Hand % Grass Cover", ylab = paste0(title.text[m], "% Grass Cover"), xlim = c(0,100), ylim = c(0,100))
    abline(0,1) # Add the 1:1 line
    
    
    # SOIL
    # Calculate Stats
    ## RMSE
    soil.rmse <- sqrt( mean( (hand.check.tot$hand.soil - hand.check.tot[ , col.number[m] + 2])^2 , na.rm = TRUE ) )
    ## R-squared
    y.mean <- mean(hand.check.tot[ , col.number[m] + 2])
    y.line <- hand.check.tot$hand.soil
    y <- hand.check.tot[ , col.number[m] + 2]
    SE.y.mean <- sum((y - y.mean)^2) # what is the standard error from the mean of y? (SMA soil cover)
    SE.y.line <- sum((y - y.line)^2) # what is the standard error from the 1:1 line? (Google soil cover)
    soil.r.sq <- 1 - SE.y.line/SE.y.mean
    correl <- cor(hand.check.tot$hand.tree, hand.check.tot[ , col.number[m] + 2])
    covar <- cov(hand.check.tot$hand.tree, hand.check.tot[ , col.number[m] + 2])
    # Plot
    plot(hand.check.tot$hand.soil*100, hand.check.tot[ , col.number[m] + 2]*100, main = paste0("Percent Soil Cover: By Hand vs. ", title.text[m], "\nRMSE = ", round(soil.rmse, 2)*100, "%  R-squared = ", round(soil.r.sq, 2), " Cor = ", round(correl, 2)), xlab = "By Hand % Soil Cover", ylab = paste0(title.text[m], "% Soil Cover"), xlim = c(0,100), ylim = c(0,100))
    abline(0,1) # Add the 1:1 line
    
  }
  
}
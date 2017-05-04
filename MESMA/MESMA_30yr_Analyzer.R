#############################  DESCRIPTION  ##########################################################
# This code is based on the older "SMA_analysis_FUNCTIONS_20170303.R" code, but is set up to analyze 
# all the MESMA hdr files in the final analysis to:
#   1. Clip the park out
#   2. Plot each year
#   3. Create graph with avg tree cover in park per year
#   4. Do tree cover change assessment




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
library(rasterVis) # needed for the gplot function
library(mapdata)
library(animation) # for making a movie at the end


#################################  Functions  ##################################################################
# Source the functions
source("F:/Dropbox/Permanent/Grad School/src_functions/src_masterfunctions.R")


######################################  CONTROL VARIABLES  ###################################################
# Define variables
save.transition.matrix <- TRUE  # Save the transition matrix?
run.transition.matrix <- TRUE
skip.those.already.plotted <- TRUE # When true, this will skip HDR files that have already been plotted (speeds things up)
save.plot <- TRUE
num.years <- 8
year.range <- c(1984:2017)
plot.res <- 1200 # Set the size/res of the plots

## Below can be made into a loop to go through each park

#################################  DIRECTORIES  #############################################################
# Main folder with all the data for a park
experiment.folder <- "F:/Dropbox/Permanent/Grad School/Projects/EleTree/analysis/MESMA/mpala"

# Folder with shape files of ground truth area
shape.folder <- paste0("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/ParkData/", park.name, "/Boundary")


# Create other directories and file names
SMA.folder <- paste0(experiment.folder, "/hdr_results/")
plot.save.folder <- paste0(experiment.folder, "/images/")
MESMA.version <- c("Laik_50_150_", "unconstrained_")
secondary.version <- "HML_all"
total.mesma.name <- paste0(MESMA.version[2], secondary.version)
new.name.append <- "_clip"

park.name <- "Mpala"


#############################  SET WORKING DIRECTORY  ######################################################################
# Set working directory to where the imagery should be saved
setwd(experiment.folder)



#######################  HDR LIST  ###########################################################################
# Get list of hdr files
hdr.list <- list.files(SMA.folder, pattern = ".hdr")
hdr.list <- hdr.list[!grepl(pattern = "class", hdr.list)] # exclude the hdr files of the classifications 


#######################  LOOP THROUGH YEARS  #################################################################

# For each year:
#   1a.Shift values according to point with 100% tree cover
#   1. Clip to boundary
#   2. Plot and save image and raster (separate loop will go through the clipped rasters)


# h <- 40
for(h in 1:length(hdr.list)){

  
  # Get file name
  MESMA.file <- substr(hdr.list[h], 0, nchar(hdr.list[h])-4) # Take off the .hdr, which isn't needed
  total.mesma.name <- gsub(pattern = "%", "prcnt", MESMA.file) # remove any % signs
  print(paste(h, "of", length(hdr.list), ":", total.mesma.name))
  
  # Skip hdr files that have already had their results plotted and the raster saved 
  if(skip.those.already.plotted == TRUE){
    plot.list <- list.files(SMA.folder)
    plot.name <- paste0("Tree_", total.mesma.name, new.name.append, ".png") # have to make the name specific, otherwise other names could register
    tif.name <- paste0("Tree_", total.mesma.name, new.name.append) # This is opportunistically created here for use at the end when saving clipped tifs
    plot.num <- grep(pattern = tif.name, plot.list) # get the numbers of the plots that have that in their names
    if(length(plot.num) > 1){
      next # got to next h 
    }
  }
  
  # Determine if unconstrained
  if(grepl(pattern = "uncon", MESMA.file)){
    is.unconstrained <- TRUE
  }else{is.unconstrained <- FALSE}
  
  # Determine if TS
  if(grepl(pattern = "TS", MESMA.file)){
    TS_Only <- TRUE
  }else{TS_Only <- FALSE}
  
  # Set the band order for cover types in the MESMA (all should be this way)
  if(TS_Only == TRUE){ # the code is being funny, so running grass as a duplicate of soil. Won't be plotted
    tr.band <- 1          
    gr.band <- 2
    so.band <- 2           
  }else{
    tr.band <- 1 
    gr.band <- 2 
    so.band <- 3 
  }
  
  
  
  #####################  LOAD SHAPEFILE and HDR  ############################################
  
  # Load the raster
  hdr <- raster(paste0(SMA.folder, MESMA.file), band = tr.band)
  
  # Shapefiles
  park.boundary <- readOGR(shape.folder, paste0(park.name, "_Boundary"))
   
  
  # Convert Mpala shapefile to lat long, which everything else is in
  park.boundary <- spTransform(park.boundary, crs(hdr))  
    
    
  # Create a list of points with 100% tree cover (manually copy and pasted these out of GoogleEarth)
  x.list <- c(304778, 304485, 309228, 308434, 304837, 299248, 299479, 311567, 311566)
  y.list <- c(41049, 41455, 42177, 41651, 38943, 44933, 45521, 37778, 37822)
  coords <- cbind(x.list, y.list)
  points <- SpatialPoints(coords)
    
  # Get the point values and the difference from 100% (the points fall at places where tree cover should be 100% all years)
  tree.cover.values <- extract(hdr, points)
  mean.tree <- mean(tree.cover.values)
  tree.dif <- 1-mean.tree
    
  # Adjust values to the 100% values (add tree.dif)  
  hdr.shifted <- hdr+tree.dif
  new.tc.values <- extract(hdr.shifted, points)
  
  # Clip out Mpala
  park <- clipTIF(tifname = hdr.shifted, clipboundary = park.boundary)
  park_unshift <- clipTIF(tifname = hdr, clipboundary = park.boundary)
  
  # Save the rasters, both shifted and unshifted
  writeRaster(park, paste0(SMA.folder, tif.name), format = "GTiff", overwrite = TRUE)
  writeRaster(park_unshift, paste0(SMA.folder, tif.name, "_unshifted"), format = "GTiff", overwrite = TRUE)

  
  # Save a plots
  # Shifted park
  png(filename = paste0(plot.save.folder, "Tree_", total.mesma.name, "_clip_shifted.png"), width = plot.res, height = plot.res)
  plot(park, main = paste0("Tree_", total.mesma.name, "_clip_shifted.png"), bty = "n", box = FALSE, axes = F)
  dev.off()
  # Shifted buffer area
  png(filename = paste0(plot.save.folder, "Tree_", total.mesma.name, "_shifted.png"), width = plot.res, height = plot.res)
  plot(hdr.shifted, main = paste0("Tree_", total.mesma.name, "_shifted.png"), bty = "n", box = FALSE, axes = F)
  plot(park.boundary, add = TRUE)
  dev.off()
  
  # Unshifted
  png(filename = paste0(plot.save.folder, "Tree_", total.mesma.name,"_clipped_raw.png"), width = plot.res, height = plot.res)
  plot(park_unshift, main = paste0("Tree_", total.mesma.name, "_clip_raw.png"), bty = "n", box = FALSE, axes = F)
  dev.off()
  # Shifted buffer area
  png(filename = paste0(plot.save.folder, "Tree_", total.mesma.name, "_raw.png"), width = plot.res, height = plot.res)
  plot(hdr.shifted, main = paste0("Tree_", total.mesma.name, "_raw.png"), bty = "n", box = FALSE, axes = F)
  plot(park.boundary, add = TRUE)
  dev.off()
  

}
  
 

##############  ANALYZE EACH YEAR  ######################################################################

#   3. Get average value and plug into table
#   4. Subtract the prior year to get annual change, save file
#   5. Run transition matrix for the first and last year (orrrrr maybe not)


#########  GETTING AVG TREE COVER AND PLUGGING INTO TABLE  ################################
# Get a list of the files
tif.list <- list.files(SMA.folder, pattern = "clip.tif")
shorter <- grep("unshifted", tif.list, invert = TRUE, value = TRUE)
shorter <- grep("tree_3ROI_clip", shorter, invert = TRUE, value = TRUE)
tif.list <- grep("TS_clip", shorter, invert = TRUE, value = TRUE)

# Get the year numbers
year.subset <- tif.list[1:num.years]
year.subset <- substr(year.subset, 28, 31)


# Build the table
mtx <- matrix(nrow = num.years, ncol = 4)
df <- as.data.frame(mtx)
names(df) <- c("year", "p44_45", "HML_all", "HML_tree")
df$year <- year.subset

i <- 17
# Loop through and add data to table
for(i in 1:length(tif.list)){
  
  # read in the tif
  tif <- raster(paste0(SMA.folder, tif.list[i]))
  avg <- cellStats(tif, median)
  
  # Get the year and file name
  if(grepl("HML_all_clip.tif", tif.list[i])){
    yr <- substr(tif.list[i], 35, 38)
    rw <- which(df$year == yr)
    cl <- which(colnames(df) == "HML_all")
    df[rw, cl] <- avg
  }else if(grepl("p44_45_TS_shadeNorm_clip.tif", tif.list[i])){
    yr <- substr(tif.list[i], 28, 31)
    rw <- which(df$year == yr)
    cl <- which(colnames(df) == "p44_45")
    df[rw, cl] <- avg
  }else if(grepl("HML_tree_3ROI_shadeNorm_clip.tif", tif.list[i])){
    yr <- substr(tif.list[i], 35, 38)
    rw <- which(df$year == yr)
    cl <- which(colnames(df) == "HML_tree")
    df[rw, cl] <- avg
  }
}

# Plot the tree cover graphs
png(filename = paste0(plot.save.folder, "HML_all.png"), width = plot.res/2, height = plot.res/3)
plot(df$year, df$HML_all, main = "HML_all", ylab = "Tree Cover", xlab = "Year", type = "b", pch = 19)
dev.off()

png(filename = paste0(plot.save.folder, "HML_tree.png"), width = plot.res/2, height = plot.res/3)
plot(df$year, df$HML_tree, main = "HML_tree", ylab = "Tree Cover", xlab = "Year", type = "b", pch = 19)
dev.off()

#Plot the two together
png(filename = paste0(plot.save.folder, "HML_tree_all_combo.png"), width = plot.res/2, height = plot.res/3)
plot(df$year, df$HML_all, main = "HML_tree (triangles) and HML_all (circles)", ylab = "Tree Cover", xlab = "Year", type = "b", pch = 19)
points(df$year, df$HML_tree, type = "b", pch = 17, col = "blue")
dev.off()


# Do the same thing for the unshifted data, to compare
# Get a list of the files
tif.list2 <- list.files(SMA.folder, pattern = "clip")
shorter2 <- grep("unshifted", tif.list2, invert = FALSE, value = TRUE)
shorter2 <- grep("tree_3ROI_clip", shorter2, invert = TRUE, value = TRUE)
tif.list2 <- grep("TS_clip", shorter2, invert = TRUE, value = TRUE)

# Get the year numbers
year.subset <- tif.list2[1:num.years]
year.subset <- substr(year.subset, 28, 31)


# Build the table
mtx <- matrix(nrow = num.years, ncol = 4)
df_unshift <- as.data.frame(mtx)
names(df_unshift) <- c("year", "p44_45", "HML_all", "HML_tree")
df_unshift$year <- year.subset

i <- 17
# Loop through and add data to table
for(i in 1:length(tif.list2)){
  
  # read in the tif
  tif <- raster(paste0(SMA.folder, tif.list2[i]))
  avg <- cellStats(tif, median)
  
  # Get the year and file name
  if(grepl("HML_all_clip_unshifted.tif", tif.list2[i])){
    yr <- substr(tif.list2[i], 35, 38)
    rw <- which(df_unshift$year == yr)
    cl <- which(colnames(df_unshift) == "HML_all")
    df_unshift[rw, cl] <- avg
  }else if(grepl("p44_45_TS_shadeNorm_clip_unshifted.tif", tif.list2[i])){
    yr <- substr(tif.list2[i], 28, 31)
    rw <- which(df_unshift$year == yr)
    cl <- which(colnames(df_unshift) == "p44_45")
    df_unshift[rw, cl] <- avg
  }else if(grepl("HML_tree_3ROI_shadeNorm_clip_unshifted.tif", tif.list2[i])){
    yr <- substr(tif.list2[i], 35, 38)
    rw <- which(df_unshift$year == yr)
    cl <- which(colnames(df_unshift) == "HML_tree")
    df_unshift[rw, cl] <- avg
  }
  
}

# Plot the tree cover graphs
png(filename = paste0(plot.save.folder, "HML_all_raw.png"), width = plot.res/2, height = plot.res/3)
plot(df_unshift$year, df_unshift$HML_all, main = "HML_all Raw", ylab = "Tree Cover", xlab = "Year", type = "b", pch = 19)
dev.off()

png(filename = paste0(plot.save.folder, "HML_tree_raw.png"), width = plot.res/2, height = plot.res/3)
plot(df_unshift$year, df_unshift$HML_tree, main = "HML_tree Raw", ylab = "Tree Cover", xlab = "Year", type = "b", pch = 19)
dev.off()

#Plot the two together
png(filename = paste0(plot.save.folder, "HML_tree_all_raw_combo.png"), width = plot.res/2, height = plot.res/3)
plot(df_unshift$year, df_unshift$HML_all, main = "Raw: HML_tree (triangles) and HML_all (circles)", ylab = "Tree Cover", xlab = "Year", type = "b", pch = 19)
points(df_unshift$year, df_unshift$HML_tree, type = "b", pch = 17, col = "blue")
dev.off()





###################   ANNUAL and TOTAL CHANGE MAPS  #########################################

# Ok, ok. So this should loop through a list of the distinguishing names of the rasters. That's
# What this and everything up above should be doing. So for Mpala, that would mean the HML_all, HML_tree and the p44_45.
# That way, what is modified is the list of those names at the beginning for new runs. 
# From here on out, I'll write it that way. But not changing what is above yet. 

file.distinguishers <- c("HML_all_clip.tif", "p44_45_TS_shadeNorm_clip.tif", "HML_tree_3ROI_shadeNorm_clip.tif")
# n <- 1
for(n in 1:length(file.distinguishers)){
  
  # Get a list of the files
  tif.list <- list.files(SMA.folder, pattern = file.distinguishers[n])
  
  # Get the year numbers
  year.subset <- findYear(possible.years = year.range, files.to.search = tif.list) # wrote this function (in src_masterfunctions.R)
  year.subset <- unique(year.subset) # should all be unique, but just in case
  year.subset <- year.subset[sort.list(year.subset)] # ensure the years are ordered
  
  
  # Loop through and create the change maps
  i <- 2
  for(i in 1:length(tif.list)){
    # Give the iteration a better name
    current.tif <- tif.list[i]
    
    # read in the tif
    tif <- raster(paste0(SMA.folder, current.tif))
    
    # Create a stack to use at the end for calculating the st.dev of each cell
    if(i == 1){
      tif.stack <- tif
    }else{
      tif.stack <- stack(tif.stack, tif)
    }
    
    # Get the year 
    yr <- findYear(year.range, current.tif) # this was purposely redundant (could just do year.subset[i])
    
    
    # Create the change maps
    if(yr == year.subset[1]){ # if it's the first year, there is no change, so just set it to blank (zero)
      year.zero <- tif # this will be the base off which all other differences are created (this will be subtracted from all subsequent rasters)
      change.map <- tif
      change.map[!is.na(change.map[])] <- 0 # Create the blank image that will start the image series
      annual.change.map <- change.map
      prior.year <- tif 
    }else if(yr == year.subset[length(year.subset)]){ # if it's the last year
      change.map <- tif - year.zero
      annual.change.map <- tif - prior.year
      total.change.map <- tif - year.zero
    }else{ # create the change map
      change.map <- tif - year.zero
      annual.change.map <- tif - prior.year
      prior.year <- tif # do this at the end so that prior year is always the year before
    }
    
    ### FIRST ROUND OF PLOTTING
    # Set the range of the plot legend
    leg.range <- c(-4, 4)
    xlims <- range(pretty(coordinates(change.map)[,1]))
    ylims <- range(pretty(coordinates(change.map)[,2]))
    
    
    # Save the plain tif
    tif.m <- gplot(tif, max = ncell(change.map)) + 
      geom_tile(aes(fill=value)) +
      ggtitle(paste0(substr(current.tif, 1, nchar(current.tif)), "_TC_PLAIN")) +
      xlim(xlims) + ylim(ylims) +  theme_void() + coord_equal() +
      scale_fill_gradientn(colours = rev(terrain.colors(255)), breaks=seq(-10,10,2),  # Set whatever breaks you want
                           limits=leg.range, na.value = "white")  # Set the same limits for each plot
    ggsave(tif.m, file = paste0(plot.save.folder, substr(current.tif, 1, nchar(current.tif)-4), "_TC_PLAIN.png"))
    
    
    # Save the change map as a png
    change.m <- gplot(change.map, max = ncell(change.map)) + 
      geom_tile(aes(fill=value)) +
      ggtitle(paste0(substr(current.tif, 1, nchar(current.tif)), "_CHANGEMAP")) +
      xlim(xlims) + ylim(ylims) +  theme_void() + coord_equal() +
      scale_fill_gradientn(colours = rev(terrain.colors(255)), breaks=seq(-10,10,2),  # Set whatever breaks you want
                           limits=leg.range, na.value = "white")  # Set the same limits for each plot
    ggsave(change.m, file = paste0(plot.save.folder, substr(current.tif, 1, nchar(current.tif)-4), "_CHANGEMAP.png"))
    
    
    # Save the annual change map
    annual.m <- gplot(annual.change.map, max = ncell(change.map)) + 
      geom_tile(aes(fill=value)) +
      ggtitle( paste0(substr(current.tif, 1, nchar(current.tif)-4), "_ANNUAL_CHANGEMAP")) +
      xlim(xlims) + ylim(ylims) +  theme_void() + coord_equal() +
      scale_fill_gradientn(colours = rev(terrain.colors(255)), breaks=seq(-10,10,2),  # Set whatever breaks you want
                           limits=leg.range, na.value = "white")  # Set the same limits for each plot
    ggsave(annual.m, file = paste0(plot.save.folder, substr(current.tif, 1, nchar(current.tif)-4), "_ANNUAL_CHANGEMAP.png"))
  }
 
  # At this point, there will be a total.change.map available, so work with it to get areas that have gone up or down in cover
  # Q1: What percent of the park has gone up/down in cover
  # First get number of cells with a value
  num.cells <- ncell(total.change.map[!is.na(total.change.map)])
  tc.increase <- round(ncell(total.change.map[total.change.map > 0]) / num.cells * 100, 1)
  tc.decrease <- round(ncell(total.change.map[total.change.map < 0]) / num.cells * 100, 1)
  tc.nochange <- round(ncell(total.change.map[total.change.map == 0]) / num.cells * 100, 1)
  
  ### PLOTS  
  # Save the total change map (there will only be one)
  tot.m <- gplot(total.change.map, max = ncell(change.map)) + 
    geom_tile(aes(fill=value)) +
    ggtitle(paste0(substr(current.tif, 1, nchar(current.tif)-4), "_TOTAL_CHANGEMAP", 
                   " \n Percent Area Increase: ", tc.increase, "% Decrease: ", tc.decrease, "% No Change: ", tc.nochange)) +
    xlim(xlims) + ylim(ylims) +  theme_void() + coord_equal() +
    scale_fill_gradientn(colours = rev(terrain.colors(255)), breaks=seq(-10,10,2),  # Set whatever breaks you want
                         limits=leg.range, na.value = "white")  # Set the same limits for each plot
  ggsave(tot.m, file = paste0(plot.save.folder, substr(current.tif, 1, nchar(current.tif)-4), "_TOTAL_CHANGEMAP.png"))
  
  
  # Calc the st.dev and save plot
  sd.TC <- calc(tif.stack, fun=sd)
  
  stdev.m <- gplot(sd.TC, max = ncell(change.map)) + 
    geom_tile(aes(fill=value)) +
    ggtitle(paste0(substr(current.tif, 1, nchar(current.tif)-4), "_StDev")) +
    xlim(xlims) + ylim(ylims) +  theme_void() + coord_equal() +
    scale_fill_gradientn(colours = rev(terrain.colors(255)), breaks=seq(-10,10,2),  # Set whatever breaks you want
                         limits=leg.range, na.value = "white")  # Set the same limits for each plot
  ggsave(stdev.m, file = paste0(plot.save.folder, substr(current.tif, 1, nchar(current.tif)-4), "_StDev.png"))
  
  
  
  ###  MOVIE  
  movie.stack <- tif.stack
  names(movie.stack) <- paste0("Tree Cover ", as.character(year.subset))
  
  # Set movie options 
  ani.options(nmax=80, ani.width=600, ani.height=600,
              ani.type ="png", outdir=plot.save.folder, imgdir=paste0(plot.save.folder, substr(file.distinguishers[n], 1, nchar(file.distinguishers[n]) - 3)),
              filename = paste0(substr(current.tif, 1, nchar(current.tif)-4), ".html") ,ani.dev=png,title="Tree Cover Change",
              description ="Tree Cover Animation", autobrowse=TRUE, interval=1)
  # Create movie
  saveHTML(animate(movie.stack), htmlfile = paste0(plot.save.folder, substr(current.tif, 1, nchar(current.tif)-4), ".html"))
}


# From here, if you want to make a confusion matrix or compare hand.tree vs the results,
# have to go to the SMA_analysis_FUNCTIONS_20170303 code to plug in the values. It's a mess


###############  ADD CHIRPS DATA  #########################################################################

# Set working directory to where CHIRPS is stored
setwd("F:/Dropbox/Permanent/Grad School/data/CHIRPS")

# How many years of data do you want?
n.years <- 2013-1984+1

# Folder with shape files of ground truth area
park.name <- "Mpala"
shape.folder <- paste0("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/ParkData/", park.name, "/Boundary")

# Read in boundary shapefile
new.boundary <- readOGR(shape.folder, paste0(park.name, "_Boundary"))


# List all the files from 1984-2013
chirps.list <- list.files(".", pattern = ".tif")


# Loop through and just keep the years you want
new.chirp <- c()
for(i in as.character(c(1984:2013))){
  temp <- grep(pattern = i, chirps.list, value = TRUE)
  new.chirp <- c(new.chirp, temp)
}


# Loop through, clip, take average and assign to table
# Build table first
mx <- matrix(nrow = n.years*12, ncol = 2)
chirps.df <- as.data.frame(mx)
names(chirps.df) <- c("date", "precip")

# Make an annual precip table
mx <- matrix(data = 0,  nrow = n.years, ncol = 2)
annual.df <- as.data.frame(mx)
names(annual.df) <- c("year", "precip")
annual.df$year <- c(1984:2013)


n <- 1
for(n in 1:length(new.chirp)){
  # Read in the raster
  prec.ras <- raster(new.chirp[n])
  
  # Clip that tif
  park.prec <- clipTIF(tifname = prec.ras, clipboundary = new.boundary)
  avg <- cellStats(park.prec, "mean")
  
  # Create the date
  prec.year <- as.numeric(substr(new.chirp[n], 11, 14))
  prec.mo <- as.numeric(substr(new.chirp[n], 15, 16))
  
  # Get the annual.df row that corresponds to the year
  an.row <- which(annual.df$year == prec.year)
  
  # Plug in the value
  chirps.df$date[n] <- substr(new.chirp[n], 11, 16)
  chirps.df$precip[n] <- avg
  chirps.df$date.frac[n] <- prec.year + (prec.mo / 12) # get the months to be fractions added to the year (for plotting)
  annual.df$precip[an.row] <- annual.df$precip[an.row] + avg
}

# Plot those!
# Shifted

# 


# Add elephant density on the same plot




png(filename = paste0(plot.save.folder, "HML_tree_all_combo_withPrecip.png"), width = plot.res/2, height = plot.res/3)
par(mar = c(4,4,4,4))
barplot(annual.df$precip, xaxt = "n", yaxt = "n", 
        xlab = NA, ylab = NA, ylim = c(0, 2000), col = "lightgray")
mtext("Annual Precipitation (mm)", side = 4, line = 2, cex = 1)
axis(side=4, at = pretty(0:2000))

par(new = T)
plot(df$year, df$HML_all, main = "HML_tree (triangles) and HML_all (circles)", ylab = "Tree Cover", xlab = "Year", type = "b", pch = 19)
points(df$year, df$HML_tree, type = "b", pch = 17, col = "blue")


dev.off()





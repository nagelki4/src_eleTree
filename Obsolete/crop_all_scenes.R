#  DESCRIPTION  #
## This script stacks and crops all the scenes as part of the workflow to create the files for MESMA

# Date: 4/5/2017
# Ryan Nagelkirk

library(raster)





# List the files
file.list <- list.files(file.loc, pattern = "band")
# What are the unique names?
landsat.names <- substr(file.list, 1, 21)
ls.unique <- unique(landsat.names)

# Get the boundary name
boundary.name <- 





# For each Landsat image, stack it, clip it, make sure values are 0-10000 and apply the cloud mask. Then save as ENVI hdr
for(i in 1:length(ls.unique)){
  # Get the 7 bands of the first image
  seven.band <- grep(ls.unique[i], file.list, value = TRUE)
  
  # Get the cloud mask
  cloud.mask <- raster(paste0(ls.unique[i], "_cfmask.tif"))
  
  # Read in and stack the seven bands
  for(t in 1:length(seven.band)){
    ras <- raster(seven.band[t])
    if(t == 1){
      seven.stack <- ras
    }else{
      seven.stack <- stack(seven.stack, ras)
    }
  }
  
  # Crop to boundary
  clip.stack <- clipTIF(tifname = seven.stack, clipboundary = site.boundary)
  clip.mask <- clipTIF(tifname = cloud.mask, clipboundary = site.boundary)
  
  
  # For every band, set anything above 10000 or below 0 to NA
  for(t in 1:dim(clip.stack)[3]){
  # values below zero or >10000 are no good
  clip.stack[[t]][clip.stack[[t]] > 10000 | clip.stack[[t]] < 0] <- NA
  
  }
  
  # Apply the cloud mask
  clip.mask[clip.mask != 0] <- NA
  mask <- clip.mask + 1 # zero values mean there is no clouds, so you add 1 so that when multiplied, those places stay the same
  # Apply mask to each layer of stack
  for(n in 1:dim(clip.stack)[3]){
    # values below zero or >10000 are no good
    clip.stack[[n]] <- (clip.stack[[n]] * mask)   
  }
  # Save the raster stack as an ENVI hdr file
  writeRaster(clip.stack, paste0(save.loc, "/", ls.unique[i], "_site_stack"), format = "ENVI", overwrite = TRUE)
}





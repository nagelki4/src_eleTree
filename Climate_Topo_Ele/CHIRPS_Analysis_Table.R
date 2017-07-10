# Get the CHIRPS data for a park
#  1. Clips out park
#  2. Gets spatial avg precip for each month in park
#  3. Determines the lowest trailing average month for each year (should be driest month aka height of drought)



# Date: 20170526 (written before, but started modifying on this date)

library(raster)
library(lubridate)

#################################  Functions  ##################################################################
# Source the functions
source("F:/Dropbox/Permanent/Grad School/src_functions/src_masterfunctions.R")

# Set working directory to where CHIRPS is stored
setwd("F:/Dropbox/Permanent/Grad School/data/CHIRPS")

# What years do you want to work with (all years btwn these will be analyzed)
first.year <- 1984
last.year <- 2017

# How many years of data do you want?
n.years <- last.year-first.year+1

# List the years
year.list <- first.year:last.year

# Folder with shape files of ground truth area
park.name <- "Mpala"
shape.folder <- paste0("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/ParkData/", park.name, "/Boundary")

# Read in boundary shapefile
park.boundary <- readOGR(shape.folder, paste0(park.name, "_Boundary"))

 

# List all the files from 1984-2013
chirps.list <- list.files(".", pattern = ".tif")
# Loop through and just keep the years you want
new.chirp <- c()
for(i in as.character(c(year.list))){
  temp <- grep(pattern = i, chirps.list, value = TRUE)
  new.chirp <- c(new.chirp, temp)
}


# Loop through, clip, take average and assign to table
# Build table first
mx <- matrix(nrow = length(new.chirp), ncol = 2)
chirps.df <- as.data.frame(mx)
names(chirps.df) <- c("month", "precip")

n <- 1
for(n in 1:length(new.chirp)){
  # Read in the raster
  prec.ras <- raster(new.chirp[n])
  
  # Clip that tif
  park.prec <- clipTIF(tifname = prec.ras, clipboundary = park.boundary)
  avg <- cellStats(park.prec, "mean")
  
  # Plug in the value
  chirps.df$month[n] <- substr(new.chirp[n], 11, 16)
  chirps.df$precip[n] <- avg 

}



#############  ADD HEIGHT OF DROUGHT MONTH  #################################################################################
# Add the month that has the lowest precip before and during it by doing a trailing weighted average
# That is, take the average of it and the 2 months before it. Whichever month scores lowest is the month recorded
# Also record that precip value 


# Create the new columns and populate with NAs
chirps.df$height_of_drought <- NA
chirps.df$height_3mo_prec_avg <- NA
chirps.df$drought_flag <- NA  # this flag is just there to help identify the months in GEE
chirps.df$landsat_yr_doy <- NA  # this is also to help comparing to landsat data
chirps.df$ymd <- NA

# For each year and each month in that year, get the average value of that month and the 2 before

# i <- 1
# For each year, get the month with the lowest trailing 3 month average
for(i in 1:n.years){
  # Create list
  temp.list <- c()
  
  # Get year
  spec.year <- year.list[i]
  
  # Start month loop here
  for(x in 1:12){
    
    # Skip the very first two months, because it won't work.
    if(i == 1 & x<=2){
      temp.list <- c(temp.list, NA)
      next
    }
    
    # Determine the row you're on
    row.num <- (i-1)*12+x
    # Calc average of month and 2 prior
    avg <- mean(chirps.df$precip[(row.num-2):row.num])
    
    # Plug into list
    temp.list <- c(temp.list, avg)
  }
  
  # Find min of list
  list.min <- min(temp.list, na.rm = T)
  
  # Which date/month corresponds?
  order.in.list <- which(temp.list == list.min)
  
  # Determine the row 
  row.num <- (i-1)*12+order.in.list
  
  # Plug that date into df corresponding to the date (same row as date), along with the avg precip value
  chirps.df$height_of_drought[row.num] <- chirps.df$month[row.num]
  chirps.df$height_3mo_prec_avg[row.num] <- list.min
  chirps.df$drought_flag[row.num] <- 1 
  
  # Convert the date to landsat year/doy format and enter
  # You want the day at the end of the month (because thats the day the 3 month precip really corresponds to), 
  # so increase the month count by 1 and then decrease the day by 1, which will get the last day of the month
  yr <- substr(chirps.df$month[row.num], 1, 4)
  mo <- substr(chirps.df$month[row.num], 5, 6)
  mo <- as.character(as.numeric(mo) + 1)
  if(nchar(mo) == 1){
    mo <- paste0("0", mo)
  }
  date.text <- paste(yr, mo, "01", sep = "-")
  doy <- yday(date.text)-1
  if(nchar(doy) < 3){
    doy <- paste0(0, doy)
  }
  yr.doy <- paste0(yr, doy)
  chirps.df$landsat_yr_doy[row.num] <- yr.doy
  chirps.df$ymd[row.num] <- as.character(ymd(date.text)-1)
}

# Write as csv
# write.csv(chirps.df, "chirps_summary_table.csv", row.names = FALSE)


###########  Refine table to something that will be easier for GEE
# A simpler table, with just one date per year will be easier to handle in GEE
# Get only the rows without NAs
chirps.filtered <- chirps.df[complete.cases(chirps.df), ]

# Create the new table
mx <- matrix(NA, nrow = length(year.list), ncol = 2)
drought.df <- as.data.frame(mx)
names(drought.df) <- c("Year", "Drought_Date")
drought.df$Year <- year.list

# Fill in the dates for each year
for(i in 1:nrow(chirps.filtered)){
  # Get the year
  yr <- substr(chirps.filtered$month[i], 1, 4)
  # Get the row in drought.df that corresponds
  drought.row <- which(drought.df$Year == yr)
  # Plug in the ymd from chirps.filtered
  drought.df$Drought_Date[drought.row] <- chirps.filtered$ymd[i]
}

# Write that new sucker
write.csv(drought.df, "drought_table.csv", row.names = FALSE)
# plot(chirps.df$precip, type = "h")


####################  THE below is from a prior script (CHIRPS_download_and_process) but was unnecessary. Might be useful here

# ##### Compute stats
# park.list <- c("Mpala") # this will need to be written differently, or could be, to just list the folders in ParkData

# # Create CHIRPS folder in each park folder
# for(n in park.list){
#   park.chirp.folder <- paste("./ParkData/", n, "/CHIRPS", sep = "")
#   dir.create(park.chirp.folder)
# }

# # Set the row counter for the df
# row.counter <- 1
# 
# for(n in park.list){
#   # n <- park.list[1]
#   park.chirp.folder <- paste("./ParkData/", n, "/CHIRPS", sep = "")
#   clip.list <- list.files(park.chirp.folder, pattern = "tif")
#   for(o in clip.list){
#     # Load the raster
#     # o <- clip.list[1]
#     clp <- raster(paste(park.chirp.folder, "/", o, sep = ""))
#     
#     # Compute the stats
#     month.min <- clp@data@min
#     month.max <- clp@data@max
#     x <- as.array(clp)
#     month.mean <- mean(x)
#     all.values <- as.list(x)
#     
#     # Determine some stats
#     year <- substr(o, 17, 20)
#     month <- substr(o, 21,22)
#     
#     # Add the min, max and mean to a df
#     if(row.counter == 1){
#       # Create the df
#       mmm <- matrix(NA, nrow = length(clip.list), ncol = 4)
#       colnames(mmm) <- c("YearMo", "Min", "Max", "Mean")
#       mmm.df <- data.frame(mmm)
#       # Assign the values
#       mmm.df$YearMo[row.counter] <- paste0(year, month)
#       mmm.df$Min[row.counter] <- month.min
#       mmm.df$Max[row.counter] <- month.max
#       mmm.df$Mean[row.counter] <- month.mean
#     }else{
#       mmm.df$YearMo[row.counter] <- paste0(year, month)
#       mmm.df$Min[row.counter] <- month.min
#       mmm.df$Max[row.counter] <- month.max
#       mmm.df$Mean[row.counter] <- month.mean
#     }
#     
#     
#     # Add all that month's values to a new column in a different df
#     if(row.counter == 1){
#       p <- data.frame(unlist(all.values))
#       names(p)[1] <- paste0(year, month)
#     }else{
#       p$new <- unlist(all.values)
#       names(p)[row.counter] <- paste0(year, month)
#     }
#     
#     row.counter <- row.counter + 1
#   }
#   # Write out the dfs to file
#   analysis.folder <- paste("./ParkData/", n, "/ANALYSIS", sep = "")
#   write.csv(p, file = paste0(analysis.folder, "/CHIRPS_All.tile.monthly.cell.values.csv"), row.names = FALSE)
#   write.csv(mmm.df, file = paste0(analysis.folder, "/CHIRPS_MinMaxMean.values.csv"), row.names = FALSE)
# }



# ##### Clip the park boundary 
# tif.filepaths <- list.files(pattern = ".tif")
# 
# # For each park, clip the rasters 
# for(j in park.list){
#   # Set plot counter
#   plot.counter <- 1
#   
#   # Identify folder to save clips
#   clipped.folder <- paste(zipFolder, "/ParkData/", j, "/CHIRPS", sep = "")
#   
#   # Load the shapefile
#   bound.folder <- paste("./ParkData/", j, "/Boundary", sep = "")
#   bound.file <- list.files(bound.folder, pattern = ".shp") # get the name of the file
#   bound.loc <- paste(bound.folder, "/", bound.file, sep = "")
#   bd <- readShapePoly(bound.loc)
#   proj4string(bd) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") # I just pulled this off the trmm file using projection(trmm)
#   ext <- extent(bd)
#   
#   # Clip the park rasters
#   for(m in tif.filepaths){
#     chirp.raster <- raster(m)
#     cr <- crop(chirp.raster, ext, snap = "out")
#     
#     # Save the raster
#     writeRaster(cr, paste(clipped.folder, "/", j, "_", m, sep = ""), overwrite = TRUE)
#     
#     if(plot.counter == 1){
#       overlay.folder <- paste("./ParkData/", j, "/CHIRPS/Overlay", sep = "")
#       dir.create(overlay.folder)
#       jpeg(paste(overlay.folder, "/", j, "_boundary_extent.jpg", sep = ""))
#       plot(chirp.raster, ext = ext+1)
#       plot(bd, add = T)
#       dev.off()
#       
#       plot.counter <- plot.counter + 1
#     }
#     
#   }
#   
# }







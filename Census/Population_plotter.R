####  DESCRIPTION
# This code goes into the elephant population csv and plots the elephant population and densities for each park. 


# Set the working directory
setwd("X:/nagelki4/Projects/EleTree/Census Numbers") # location with the census csv

# Look at files
list.files()

# Load the file
ele.num <- read.csv("GEC_20170127_data_added.csv")
list.of.strata <- read.csv("List_of_strata.csv")

# Get some stats for plotting
max.ele <- max(ele.num$Estimate)

# Create a list of the unique Ecosystem/Stratum combinations
eco.strat <- unique(ele.num[, 2:3])




# # Assign the ecosystem and stratum <- this could be written to plot all of them by doing the unique() of the column values
# eco <- c("Masai Mara", "Masai Mara", "Kruger NP", "Limpopo NP", "Luangwa", "Luangwa", "Niassa NR", 
#          "Ruaha-Rungwa", "Selous-Mikumi", "Serengeti", "Laikipia-Samburu", "Northern Botswana", "Northern Botswana", "Northern Botswana")
# strat <- c("Masai Mara R", "unprotected areas", "Kruger NP", "Limpopo NP", "Luangwa North NP", "Luangwa South NP", "Niassa NR",
#            "Ruaha-Rungwa", "Selous-Mikumi", "Serengeti", "Laikipia-Samburu", "Ngamiland", "Chobe district", "Central district" )

# eco <- "Masai Mara" # this was done to get a plot of all the data for Tsavo-Amb
i <- 2
for(i in 1:nrow(eco.strat)){
  eco <- eco.strat[[1]][i]
  strat <- eco.strat[[2]][i]
  
  # List where those values appear in those columns
  tf <- ele.num$Ecosystem == eco & ele.num$Stratum == strat
  tf2 <- list.of.strata$Ecosystem == eco & list.of.strata$Stratum == strat
  
  # Subset the df based on those TF
  nums <- ele.num[tf, ]
  park.area <- list.of.strata[tf2, ]
  
  # Get the park area
  p.area <- park.area$Area..km2.
  
  # Check for "/" and replace with "_" as necessary
  eco <- gsub("/", "_", eco)
  strat <- gsub("/", "_", strat)
  
  nums$Ecosystem <- eco
  nums$Stratum <- strat
  
  # Make a linear model
  ele.mod <- lm(nums$Estimate ~ nums$Year)
  
  # Check to make sure more than one year of data is present. If not, can't create a lm
  num.years <- nrow(nums)
  
  # If more than one year, create the plot
  if(num.years > 1){
    # Plot and save the location you want
    png(paste0(eco, "_", strat,".png"))
    par(mar = c(4,4,4,4))
    plot(nums$Year, nums$Estimate, main = paste0(nums$Country[1], ", ", eco, ", ", strat), 
         ylim = c(0, 80000), xlim = c(1990, 2016), ylab = "# of Elephants", xlab = "Year", type = "b")
    abline(ele.mod)
    
    # Add elephant density on the same plot
    par(new = T)
    plot(nums$Year, nums$Estimate/p.area, type = "b", xaxt = "n", yaxt = "n", 
         xlab = NA, ylab = NA, ylim = c(0, 5), xlim = c(1990, 2016), col = "red")
    mtext("Elephant Density (num/km^2)", side = 4, line = 2, cex = 1, col = "darkred")
    axis(side=4, at = pretty(0:5))
  
    dev.off()
  }
}



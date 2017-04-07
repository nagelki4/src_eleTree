#  This is for helping to build the libraries for the MESMA



# Set the working directory
setwd("X:/nagelki4/Projects/EleTree/analysis/MESMA/mpala/spectral_libraries")

# What file?
file.name <- "mpala_2013271_threeROI.csv"

# Read in the file
csv <- read.csv(file.name, header = TRUE)

# Change the column names
colnames(csv) <- c("Name", "Cover", "Date")


# Add the values
for(i in 1:nrow(csv)){
  csv$Cover[i] <- substr(csv$Name[i], 1, 4)
}
# Add the date
csv$Date <- "20170406"

# Save back to same name
write.csv(csv, file.name, row.names = FALSE, quote = FALSE)


#  This is for helping to build the libraries for the MESMA



# Set the working directory
setwd("C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/analysis/mesma/mpala/spectral_libraries")

# What file?
file.name <- "mpala_1984240_singleROI.csv"

# Read in the file
csv <- read.csv(file.name, header = TRUE)


#############  ADD MEtADATA  ######################################################################

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


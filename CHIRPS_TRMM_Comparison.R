# Compare CHIRPS and TRMM

fileLocation = "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis"
setwd(fileLocation)

# Bring in the CHIRPS and TRMM dfs
load("./ANALYSIS/CHIRPS_ParkSummaries.Rdata")
load("./ANALYSIS/TRMM_ParkSummaries.Rdata")

# Figure out how to do summary stats and graph with the list of values in the "values column"
# Will have to loop through each row and pick out the name of the month and year and get the average value for a new table, or plot the histogram
# for that month using that current row

# This will give the values which you can now subset doing x[]
x <- unlist(CHIRPS_df$values[1]$Elements)



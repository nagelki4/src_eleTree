# moving files
# these are CHUNKS OF CODE and it isn't written too all run as one code


#####################  Deleting Unnecessary Files  ############################################

setwd("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/Landsat")


# delete all unnecesary files
# Get a list of all the files that are on F:
unwanted.names <- c("radsat", "sensor", "solar", "aerosol", "cloud_qa", "opacity") # could also do "cloud_qa" and "opacity"
tot.list <- c()
for(t in unwanted.names){
  unwanted.files <- list.files(".", pattern = t, recursive = TRUE)
  tot.list <- c(tot.list, unwanted.files)
}

# now delete them all!!!!
# file.remove(tot.list)

######################################################################################

###########  MOVE ALL .gz FILES OFF DROPBOX AND ONTO DESKTOP  ########################
setwd("F:/Dropbox/Permanent/Grad School/Projects/EleTree/data/Landsat")

# Get a list of all the files that are on F:
landsat.gz.list <- list.files(".", pattern = ".tar.gz", recursive = TRUE, full.names = TRUE)
just.names <- list.files(".", pattern = ".tar.gz", recursive = TRUE)

new.loc <- "C:/Users/nagelki-4/Desktop/nagelki4/Grad School/Projects/EleTree Analysis/Landsat/Originals"


# file.rename(landsat.gz.list, paste0(new.loc, "/", just.names))















# Get a list of all the files that are on F:
landsat.gz.list <- list.files(".", pattern = ".tar.gz", recursive = TRUE)
# Don't include the ones that are partial downloads
incomplete.files <- grep(".dtapart", landsat.gz.list, value = TRUE)

# Delete all the incomplete files (have checked that the complete files exist)
# file.remove(incomplete.files)

# These are the complete files
f.gz.complete <- landsat.gz.list[-incomplete.files]
# Get just the file names
f.gz.complete <- substr(f.gz.complete, nchar(f.gz.complete)-45, nchar(f.gz.complete))


# These are the incomplete files
incomplete.files <- grep(".dtapart", landsat.gz.list, value = TRUE)
# Get the file name in these
incomplete.complete <- substr(incomplete.files, nchar(incomplete.files)-92, nchar(incomplete.files)-47)


missing.files <- ""
# So what have some of the other in it?
for(i in 1:length(incomplete.complete)){
  if(length(grep(incomplete.complete[i] ,f.gz.complete)) == 0){
    missing.files <- c(missing.files, grep(incomplete.complete[i], f.gz.complete, value = TRUE))
  }
}


## The above was changed to check if all the partial files were now in the folders as complete files


# Get just the file names
f.gz.complete <- substr(f.gz.complete, nchar(f.gz.complete)-45, nchar(f.gz.complete))

# Get a list of the files on C:
c.path <- "C:/Users/nagelki-4/Dropbox/Permanent/Grad School/Projects/EleTree/data/Landsat"
c.files <- list.files(c.path, pattern = "tar.gz")

incomplete.files <- grep(".dtapart", c.files)
c.files.complete <- c.files[-incomplete.files]


# Loop through the files on C: and check if they are on F:
# If not on F: then file.copy and move to F; Landsat folder

# Get a count of the files first
count <- 0
for(i in c.files.complete){
  if(length(grep(i, f.gz.complete)) == 0){
    count <- count + 1
  }
}

i <- c.files.complete[1]
for(i in c.files.complete){
  if(length(grep(i, f.gz.complete)) == 0){
    file.copy(paste0(c.path, "/", i), paste0("./", i))
  }
}

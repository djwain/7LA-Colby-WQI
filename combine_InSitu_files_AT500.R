# This script takes all AquaTroll 500 files in a given year folder,
# trims the upcast, and combines them into one big csv for contouring
# and other analyses.
# Note that for AquaTroll 400 files, combine_InSitu_files should be used instead
# DJW 7/25/19
# DJW 12/28/21 Cleaning up

# Load libraries
library(stringr)
library(dplyr)
library(rstudioapi)
library(janitor)
library(lubridate)

# Set site details
site <- 'SPDEP1'
year <- 2021

# Select folder where data is
datapath <- selectDirectory()

# Get list of files
filenames <- dir(datapath)

# Create output file
basefile <- paste(site,year,sep="_")
outfile <- ""
outfilename <- paste(datapath,paste(basefile,"_InSitu.csv",sep = ""),sep = "")

# Cycle through files
for(i in 1:length(filenames)){
  fullpath <- paste(datapath,filenames[i],sep = "/")
  
  # Sometimes the headers vary. So we need to find the beginning of the data
  # before reading it in. The first column header is always "Date Time"
  junk <- read.csv(fullpath, header = FALSE, stringsAsFactors = FALSE)
  skipno <- which(junk == "Date Time") - 1

  # Read in the data and clean up the column names
  dat <- read.csv(fullpath, skip = skipno, header = TRUE, stringsAsFactors = FALSE)  
  
  # Add a Year Day column for compatibility across platforms
  yr0 = as.numeric(year) - 1
  dat2$YearDay <- as.numeric(as.Date(dat$d_t,"%m/%d/%y")-as.Date(0, origin=paste(yr0,"12-31",sep = '-')))
  
  # The serial numbers (specific to each probe) are part of the column names.
  # To combine, we need to rename them.
  z_m <- dat[,grepl( "Depth" , names( dat ) ) ]
  lat <- dat[,grepl( "Latitude" , names( dat ) ) ]
  lon <- dat[,grepl( "Latitude" , names( dat ) ) ]
  T_C <-  dat[,grepl( "Temperature...C...754" , names( dat ) ) ]
  DO_ppm <-  dat[,grepl( "RDO.C" , names( dat ) ) ]
  DO_sat <-  dat[,grepl( "RDO.S" , names( dat ) ) ]
    
  
  
  # Rename columns with data I need so that it doesn't matter what instrument is being used
  z_m <- dat[,grepl( "Depth" , names( dat ) ) ]
  
  # Trim data so only downcast is used
  maxdepi <-which.max(z_m)
  dat <- dat[1:maxdepi,]
  z_m <- z_m[1:maxdepi]
  
  T_C <-  dat[,grepl( "Temp" , names( dat ) ) ]
  T_C <- T_C[,1]
  
  DO_ppm <-  dat[,grepl( "RDO" , names( dat ) ) ]
  DO_ppm <- DO_ppm[,1]
  
  YearDay <- dat$YearDay
  
  dat1 <- cbind(YearDay,z_m,T_C,DO_ppm)

  
  outfile <- rbind(outfile, dat1)
  
} 
  
rownames(outfile) <- NULL

write.csv(outfile, outfilename)
write.csv(outfile, outfilename2)


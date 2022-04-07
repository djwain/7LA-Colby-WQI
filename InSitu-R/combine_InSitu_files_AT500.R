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
site <- 'LPDEP1'
year <- 2021

# Designate where you want the file to go (user-specific)
outpath <-'/Users/djw56/Dropbox (Personal)/My Mac (ens-help’s MacBook Pro)/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Long Pond/RawData/InSitu/LPDEP1/All'

# Select folder where data is
datapath <- selectDirectory()

# Get list of files
filenames <- dir(datapath)

# Create output file
basefile <- paste(site,year,sep="_")
outfile <- ""
outfilename <- paste(outpath,paste(basefile,"_InSitu.csv",sep = ""),sep = "")

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
  YearDay <- as.numeric(as.Date(dat$Date.Time,"%m/%d/%y")-as.Date(0, origin=paste(yr0,"12-31",sep = '-')))
  
  # The serial numbers (specific to each probe) are part of the column names.
  # To combine files from both probes, we need to rename them.
  Z_m <- dat[,grepl( "Depth" , names( dat ) ) ]
  Lat <- dat[,grepl( "Latitude" , names( dat ) ) ]
  Lon <- dat[,grepl( "Latitude" , names( dat ) ) ]
  T_C <-  dat[,grepl( "Temperature...C...754" , names( dat ) ) ]
  DO_ppm <-  dat[,grepl( "RDO.C" , names( dat ) ) ]
  DO_sat <-  dat[,grepl( "RDO.S" , names( dat ) ) ]
  Turb_NTU <- dat[,grepl( "Turbidity" , names( dat ) ) ]
  TSS_ppm <- dat[,grepl( "Suspended" , names( dat ) ) ]
  AcCond_uScm <- dat[,grepl( "Actual" , names( dat ) ) ]
  SpCond_uScm <- dat[,grepl( "Specific" , names( dat ) ) ]
  Sal_psu <- dat[,grepl( "Salinity" , names( dat ) ) ]
  TDS_ppt <- dat[,grepl( "Dissolved" , names( dat ) ) ]
  Dens_gcm3 <- dat[,grepl( "Density" , names( dat ) ) ]
  Chla_RFU <- dat[,grepl( "Fluorescence" , names( dat ) ) ]
  Chla_ugL <- dat[,grepl( "a.Conc" , names( dat ) ) ]
  
  # Recombine into new dataframs
  dat1 <- cbind(YearDay,Z_m,Lat,Lon,T_C,DO_ppm,DO_sat,Turb_NTU,TSS_ppm,AcCond_uScm,
                SpCond_uScm,Sal_psu,TDS_ppt,Dens_gcm3,Chla_RFU,Chla_ugL)
  
  # Trim data so only downcast is used
  maxdepi <-which.max(Z_m)
  dat1 <- dat1[1:maxdepi,]

  outfile <- as.data.frame(rbind(outfile, dat1))
  
} 
  
rownames(outfile) <- NULL
write.csv(outfile, outfilename)



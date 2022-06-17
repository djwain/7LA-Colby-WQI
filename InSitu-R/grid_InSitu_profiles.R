# This script will take the InSitu profles, and bin averages onto a grid.
# We will use a 1 m grid to compile data for GPWMP
# DJW 12/17/19

library(stringr)
library(tidyverse)

lake <- 'Great Pond'
site <- 'GPDEP1'
year <- '2015'
dz <- 1 #grid size

basefile <- paste(paste(site,year,sep="_"),'InSitu',sep="_")

# Load data
folder <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/RawData/InSitu/")
datapath <- paste(folder,year,sep = "","/") 
filename <- paste(paste(datapath,basefile,sep = ""),".csv",sep="")

# Create an output files
outfile <-""
outfilename <- paste(datapath,paste(basefile,"_griddedTO.csv",sep = ""),sep = "")

# Read data file
dat <- read.csv(filename, skip = 0, header = TRUE, stringsAsFactors = FALSE)
dat <- dat[-1,] #First element is NA

# Extract days for which we have profiles
serialdate <- dat$Created
days <- unique(serialdate)

# Creat grid
maxdepth <- max(dat$Depth..m.)
grid <- seq(0, maxdepth, by=dz)

T  <- matrix(data=NA,nrow=length(grid),ncol=length(days))

# Loop through days and make gridded profiles
for(j in 1:length(days)){
  prof <- filter(dat,Created == days[j])
  for(i in 1:length(grid)){
    temp <- filter(prof,Depth..m.<=grid[i]+dz/2 & Depth..m.>=grid[i]-dz/2)
    T[i,j] <- mean(temp$Temp..C.)
  }
}

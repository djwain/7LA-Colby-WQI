# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load P data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Temp_DO.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Long Pond'
MIDAS1 <- 5272
DO <- dat %>% filter(MIDAS == MIDAS1)
DO1 <- DO %>% filter(STATION == 2)
DO1 <- distinct(DO1)
DO1$OXYGEN <- as.numeric(DO1$OXYGEN)
DO1$TEMPERATURE <- as.numeric(DO1$TEMPERATURE)
DO1$DEPTH <- as.numeric(DO1$DEPTH)
DO2 <- aggregate(OXYGEN~Date+DEPTH, mean, data=DO1)
T2 <- aggregate(TEMPERATURE~Date+DEPTH, mean, data=DO1)
LSM <- merge(T2, DO2)
names(LSM) <- make.names(names(LSM))

# Designate where you want the file to go (user-specific)
outpath <-'/Users/djw56/Dropbox (Personal)/My Mac (ens-help’s MacBook Pro)/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Long Pond/RawData/LSM/'
site <- 'LPDEP2'
outfilename <- paste(outpath,paste(site,"_LSM.csv",sep = ""),sep = "")
write.csv(LSM, outfilename, row.names = FALSE)



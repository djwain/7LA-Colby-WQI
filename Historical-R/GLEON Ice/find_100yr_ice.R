# This script plots the ice out date for all of the Belgrade Lakes from the LSM Data
# DJW 06APR20

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)


# Find ice off data from these MIDAS numbers
Ice <- read.csv("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/Ice Data Spreadsheet combined v2.csv", header = TRUE, stringsAsFactors = FALSE)

# Change format of year in ice data
a <- mdy(Ice$Ice.Out)
Ice$Year <- year(a)

Ice1 <- Ice %>% filter(!is.na(Ice$Year))

Ice2 <- Ice1[!duplicated(Ice1[1:2]),] %>% group_by(MIDAS) %>% summarize(npts = n(), yr_start = min(Year))

Ice100 <- Ice2 %>% filter(yr_start <= 1920)

# Extract data for Sapna

Morph <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Morphology/MaineLakes_Geography_Morphometry.xls", sheet = 2)
MIDAS100 <- c(5448, 3814, 5780, 3446, 12, 3796, 5492,70)

# Subset all data to export for further analysis
MorphIce <- subset(Morph, `Lake Code (numeric)` %in% MIDAS100)
write.csv(MorphIce, file = "MorphIce.csv",row.names=FALSE) 

# Write ice data to separate csv files

for (val in MIDAS100) {
  icetemp <- Ice1 %>% filter(MIDAS == val)
  lakename <- substr(icetemp$Lake..Town[1], start = 1, stop = 4)
  write.csv(icetemp, file = paste(lakename,"_ice.csv",sep=""),row.names=FALSE) 
}







# This script extracts the Temp/DO profiles
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Set wd
setwd("~/Documents/GitHub/7LA-Colby-WQI/Historical-R/Great Pond")


# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Temp_DO.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Great Pond'
site <- 'GP2'
MIDAS1 <- 5274
st <- 2

DO <- dat %>% filter(MIDAS == MIDAS1)
DO1 <- DO %>% filter(STATION == st)
DO1 <- distinct(DO1)
DO1$OXYGEN <- as.numeric(DO1$OXYGEN)
DO1$TEMPERATURE <- as.numeric(DO1$TEMPERATURE)
DO1$DEPTH <- as.numeric(DO1$DEPTH)
DO2 <- aggregate(OXYGEN~Date+DEPTH, mean, data=DO1)
T2 <- aggregate(TEMPERATURE~Date+DEPTH, mean, data=DO1)
LSM <- merge(T2, DO2)
names(LSM) <- make.names(names(LSM))

# Designate where you want the file to go (user-specific)
outfilename <- paste(site,"_TDO.csv",sep = "")
write.csv(LSM, outfilename, row.names = FALSE)



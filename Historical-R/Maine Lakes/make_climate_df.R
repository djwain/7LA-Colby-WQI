# Create data frame for climate change analysis
# DJW 4/24/21

# Create data frome that contains all
# the NOAA NDCC weather parameter averages for April - October
# AvgT = average temperature (C)
# MaxT = maximum temperature (C)
# MinT = minimum temperature (C)
# Prec = precipitation (mm)
# CDD = cooling degree days (C-days)
# HDD = heating degree days (C-days)
# PDSI = Palmer drought severity index
# PHDI = Palmer hydrological drought index
# PMDI = Palmer modified drought index
# PZI = Palmer Z-index

# Load packages
library(tidyverse)

datapath <- "~/Documents/Research/7LA-Colby/Belgrade Lakes/Climate/"

# Load AvgT data
filenames0 <- dir(datapath, pattern="1702-tavg-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 4,header = TRUE, stringsAsFactors = FALSE))
  
# Parse data into year and month columns
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
  
# Convert to Celcius
dat0$AvgT <- round((dat0$Value - 32)*5/9,1)
  
# Select columns we want
climate <- dat0 %>% select(Year, Month,AvgT)

# Load and join MaxT
filenames0 <- dir(datapath, pattern="1702-tmax-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 4,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$MaxT <- round((dat0$Value - 32)*5/9,1)
dat1 <- dat0 %>% select(Year, Month,MaxT)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# Load and join MinT
filenames0 <- dir(datapath, pattern="1702-tmin-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 4,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$MinT <- round((dat0$Value - 32)*5/9,1)
dat1 <- dat0 %>% select(Year, Month,MinT)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# Load and join Prec
filenames0 <- dir(datapath, pattern="1702-pcp-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 4,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$Prec <- round(dat0$Value*25.4,1)
dat1 <- dat0 %>% select(Year, Month, Prec)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# Load and join CDD
filenames0 <- dir(datapath, pattern="1702-cdd-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 4,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$CDD <- round(dat0$Value*5/9,0)
dat1 <- dat0 %>% select(Year, Month, CDD)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# Load and join HDD
filenames0 <- dir(datapath, pattern="1702-hdd-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 4,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$HDD <- round(dat0$Value*5/9,0)
dat1 <- dat0 %>% select(Year, Month, HDD)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# Load and join PDSI
filenames0 <- dir(datapath, pattern="1702-pdsi-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 3,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$PDSI <- dat0$Value
dat1 <- dat0 %>% select(Year, Month, PDSI)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# Load and join PHDI
filenames0 <- dir(datapath, pattern="1702-phdi-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 3,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$PHDI <- dat0$Value
dat1 <- dat0 %>% select(Year, Month, PHDI)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# Load and join PMDI
filenames0 <- dir(datapath, pattern="1702-pmdi-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 3,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$PMDI <- dat0$Value
dat1 <- dat0 %>% select(Year, Month, PMDI)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# Load and join PZI
filenames0 <- dir(datapath, pattern="1702-zndx-1-")
fullpath0 <- paste(datapath,filenames0,sep = "")
dat0 <- do.call(rbind,lapply(fullpath0, read.csv, skip = 3,header = TRUE, stringsAsFactors = FALSE))
dat0$Year <- as.numeric(substr(dat0$Date,1,4))
dat0$Month <- as.numeric(substr(dat0$Date,5,6))
dat0$PZI <- dat0$Value
dat1 <- dat0 %>% select(Year, Month, PZI)
climate <- merge(climate,dat1, by.x = c("Year","Month"))

# This script scrapes the DEP data for secchi disk readings, combines it with the Colby data
# and does a trend analysis on the entire dataset and the last 10 years following the DEP
# code per JD
# DJW 20MAY20

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)   
library(stringr)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Temp_DO.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 5274
SDT_LSM <- dat %>% filter(MIDAS == MIDAS1)

# Load data from Colby
lake <- 'Great Pond'
site <- 'GPDEP1'
years <- 2015:2019
filepathC <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/Transparency/")
filename1 <- paste(filepathC,site,sep=""," - Secchi 2015-2019.xlsx")
dat1 <- ""
for(i in 1:length(years)){
  temp <- read_xlsx(filename1, sheet = as.character(years[i]))
  temp$Date <- as.character(temp$Date)
  dat1 <- rbind(dat1,temp)
}
dat1$STATION <- 1

site <- 'GPDEP2'
filename2 <- paste(filepathC,site,sep=""," - Secchi 2015-2019.xlsx")
dat2 <- read_xlsx(filename2)
dat2$STATION <- 2




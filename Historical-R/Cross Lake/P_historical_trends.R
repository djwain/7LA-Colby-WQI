# This script scrapes the DEP data for phosphorus measurements
# and does a trend analysis on the entire dataset and the last 10 years 
# DJW 20MAY20

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)   
library(stringr)
library(ggplot2)
library(broom)
library(Kendall)

# Load P data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Phosphorus.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 9931
P <- dat %>% filter(MIDAS == MIDAS1)
P1 <- P %>% filter(Station == 1)

nq <- P1 %>% group_by(Qualifier) %>% summarize(npts = n())

# Find epicore data
PEC <- P1 %>% filter(Qualifier == 'EC')

# Make a month column
a <- ymd(PEC$Date)
PEC$Month <- month(a)
PEC$Year <- year(a)

# Find number of samples in each month
nmo <- PEC %>% group_by(Month) %>% summarize(npts = n())

# Start with just August P
PEC <- PEC %>% filter(Month == 8 | Month == 9 )

# AVerage data from same day
Pday<- PEC %>% group_by(Date) %>% summarise(daymean = mean(`Total P`, na.rm = TRUE))
a = ymd(Pday$Date)  
Pday$YEAR <- year(a)
Pday$MONTH <- month(a)  

# Average data from the same month in the same year
PECavg <- Pday %>% group_by(YEAR) %>% summarise(yrmean = mean(daymean, na.rm = TRUE))
PECavg10 <- PECavg %>% filter(YEAR >= 2015)

TP <- PECavg$yrmean
mk<-MannKendall(TP)
Tmean <- mean(TP)
Tmedian <- median(TP)
Tmin<- min(TP)
Tmax <- max(TP)

ggplot(PECavg, aes(Year, yrmean)) + 
  geom_point() +
  labs(y = "TP (ppb)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(lim = c(0,30)) +
  labs(title = 'Togus-1 Epicore, Aug-Sep Avg')
dev.print(png,"Togus Avg TP.png", res = 300, width = 1000)

mk10<-MannKendall(PECavg10$yrmean)
Tmean10 <- mean(PECavg10$yrmean)
Tmedian10 <- median(PECavg10$yrmean)
Tmin10 <- min(PECavg10$yrmean)
Tmax10 <- max(PECavg10$yrmean)

ggplot(PECavg10, aes(Year, yrmean)) + 
  geom_point() +
  labs(y = "TP (ppb)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(lim = c(0,30)) +
  labs(title = 'Togus-1 Epicore, Aug-Sep Avg')
dev.print(png,"Togus Avg TP 5.png", res = 300, width = 1000)

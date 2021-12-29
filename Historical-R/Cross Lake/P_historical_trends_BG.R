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

# Find bottom data
PBG <- P1 %>% filter(Depth >= 13)

# Make a month column
a <- ymd(PBG$Date)
PBG$Month <- month(a)
PBG$Year <- year(a)

# Find number of samples in each month
nmo <- PBG %>% group_by(Month) %>% summarize(npts = n())

# Start with just August P
PBG <- PBG %>% filter(Month == 8 | Month == 9 )

# AVerage data from same day
Pday<- PBG %>% group_by(Date) %>% summarise(daymean = mean(`Total P`, na.rm = TRUE))
a = ymd(Pday$Date)  
Pday$YEAR <- year(a)
Pday$MONTH <- month(a)  

# Average data from the same month in the same year
PBGavg <- Pday %>% group_by(YEAR) %>% summarise(yrmean = mean(daymean, na.rm = TRUE))
PBGavg10 <- PBGavg %>% filter(YEAR >= 2015)

TP <- PBGavg$yrmean
mk<-MannKendall(TP)
Tmean <- mean(TP)
Tmedian <- median(TP)
Tmin<- min(TP)
Tmax <- max(TP)

ggplot(PBGavg, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(y = "TP (ppb)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(lim = c(0,500)) +
  labs(title = 'Togus-1 Bottom, Aug-Sep Avg')
dev.print(png,"Togus Avg TP BG.png", res = 300, width = 1000)

mk10<-MannKendall(PBGavg10$yrmean)
Tmean10 <- mean(PBGavg10$yrmean)
Tmedian10 <- median(PBGavg10$yrmean)
Tmin10 <- min(PBGavg10$yrmean)
Tmax10 <- max(PBGavg10$yrmean)

ggplot(PBGavg10, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(y = "TP (ppb)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(lim = c(0,500)) +
  labs(title = 'Togus-1 Bottom, Aug-Sep Avg')
dev.print(png,"Togus Avg TP BG5.png", res = 300, width = 1000)

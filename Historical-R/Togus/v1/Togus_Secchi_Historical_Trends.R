# This script scrapes the DEP data for secchi disk readings
# and does a trend analysis on the entire dataset and the last 10 years following the DEP
# code per JD
# DJW 20MAY20

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)   
library(stringr)
library(ggplot2)
library(Kendall)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Secchi_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 9931
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

Secchi<- select(SDT_LSM,'DATE','SECCHI DEPTH','STATION')

a = ymd(Secchi$DATE)  
Secchi$YEAR <- year(a)
Secchi$MONTH <- month(a)  

nst <- Secchi %>% group_by(STATION) %>% summarize(npts = n())
# Just use Station 1 (363 pts vs 28 at 2)

Secchi1 <- Secchi %>% filter(STATION == 1)
nyr <- Secchi1 %>% group_by(YEAR) %>% summarize(npts = n())
nmo <- Secchi1 %>% group_by(MONTH) %>% summarize(npts = n())

# Use May-Oct
S <- Secchi1 %>% filter(MONTH >= 5 & MONTH <= 10)
S$`SECCHI DEPTH` <- as.numeric(S$`SECCHI DEPTH`)

# AVerage data from same day
Sday<- S %>% group_by(DATE) %>% summarise(daymean = mean(`SECCHI DEPTH`, na.rm = TRUE))
a = ymd(Sday$DATE)  
Sday$YEAR <- year(a)
Sday$MONTH <- month(a)  

# Average data from the same month in the same year
Savg <- aggregate(daymean~YEAR+MONTH, mean, data=Sday)

# Compute year average
Syr <- Savg %>% group_by(YEAR) %>% summarise(yrmean = mean(daymean, na.rm = TRUE))

mk<-MannKendall(Syr$yrmean)
Tmean <- mean(Syr$yrmean)
Tmedian <- median(Syr$yrmean)
Tmin<- min(Syr$yrmean)
Tmax <- max(Syr$yrmean)

ggplot(Syr, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(title = "Secchi Depth", y = "SDT (m)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)) +
 labs(title = 'Togus-1 Secchi, May-Oct Avg')
dev.print(png,"Togus Avg Secchi.png", res = 300, width = 1000)

S10 <- filter(Syr,YEAR >= 2015)
mk10<-MannKendall(S10$yrmean)
Tmean10 <- mean(S10$yrmean)
Tmedian10 <- median(S10$yrmean)
Tmin10 <- min(S10$yrmean)
Tmax10 <- max(S10$yrmean)

ggplot(S10, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(title = "Secchi Depth", y = "SDT (m)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)) +
  labs(title = 'Togus-1 Secchi, May-Oct Avg')
dev.print(png,"Togus Avg Secchi 10.png", res = 300, width = 1000)



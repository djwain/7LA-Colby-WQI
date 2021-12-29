# This script scrapes the DEP data for CondosCondorus measurements
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

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_pHColorCond_Alk_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 9931
Cond <- dat %>% filter(MIDAS == MIDAS1)
Cond1 <- Cond %>% filter(Station == 1)

# Make a month Condumn
a <- ymd(Cond1$Date)
Cond1$Month <- month(a)
Cond1$Year <- year(a)

# Find number of samples in each month
Cond1 %>% group_by(Month) %>% summarize(npts = n())

# Start with just August Cond
Cond1 <- Cond1 %>% filter(Month == 8 | Month == 9 )

# AVerage data from same day
Cday<- Cond1 %>% group_by(Date) %>% summarise(daymean = mean(`Conductivity (uS)`, na.rm = TRUE))
a = ymd(Cday$Date)  
Cday$YEAR <- year(a)
Cday$MONTH <- month(a)  

# Average data from the same month in the same year
Cavg <- Cday %>% group_by(YEAR) %>% summarise(yrmean = mean(daymean, na.rm = TRUE))
Cavg10 <- Cavg %>% filter(YEAR >= 2015)

C <- Cavg$yrmean
mk<-MannKendall(C)
Tmean <- mean(C, na.rm = TRUE)
Tmedian <- median(C, na.rm = TRUE)
Tmin<- min(C,na.rm = TRUE)
Tmax <- max(C,na.rm = TRUE)

ggplot(Cavg, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(y = "Cond (uS)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(lim = c(0,90)) +
  labs(title = 'Togus-1 Conductivity, Aug-Sep Avg')
dev.print(png,"Togus Avg Cond.png", res = 300, width = 1000)

mk10<-MannKendall(Cavg10$yrmean)
Tmean10 <- mean(Cavg10$yrmean)
Tmedian10 <- median(Cavg10$yrmean)
Tmin10 <- min(Cavg10$yrmean)
Tmax10 <- max(Cavg10$yrmean)

ggplot(Cavg10, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(y = "Cond (uS)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(lim = c(0,90)) +
  labs(title = 'Togus-1 Conductivity, Aug-Sep Avg')
dev.print(png,"Togus Avg Cond 5.png", res = 300, width = 1000)
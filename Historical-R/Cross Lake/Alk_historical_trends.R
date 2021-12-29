# This script scrapes the DEP data for AlkosAlkorus measurements
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
Alk <- dat %>% filter(MIDAS == MIDAS1)
Alk1 <- Alk %>% filter(Station == 1)

# Make a month Alkumn
a <- ymd(Alk1$Date)
Alk1$Month <- month(a)
Alk1$Year <- year(a)

# Find number of samples in each month
Alk1 %>% group_by(Month) %>% summarize(npts = n())

# Start with just August Alk
Alk1 <- Alk1 %>% filter(Month == 8 | Month == 9 )

# AVerage data from same day
Cday<- Alk1 %>% group_by(Date) %>% summarise(daymean = mean(`Alkalinity (mg/L)`, na.rm = TRUE))
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
  labs(y = "Alk (mg/L)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(lim = c(0,20)) +
  labs(title = 'Togus-1 Alkalinity (mg/L), Aug-Sep Avg')
dev.print(png,"Togus Avg Alk.png", res = 300, width = 1000)

mk10<-MannKendall(Cavg10$yrmean)
Tmean10 <- mean(Cavg10$yrmean)
Tmedian10 <- median(Cavg10$yrmean)
Tmin10 <- min(Cavg10$yrmean)
Tmax10 <- max(Cavg10$yrmean)

ggplot(Cavg10, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(y = "Alk (mg/L)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(lim = c(0,20)) +
  labs(title = 'Togus-1 Alkalinity (mg/L), Aug-Sep Avg')
dev.print(png,"Togus Avg Alk 5.png", res = 300, width = 1000)
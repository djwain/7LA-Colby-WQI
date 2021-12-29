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
filename <- paste(filepath,"MaineLakes_Temp_DO.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 9931
DO <- dat %>% filter(MIDAS == MIDAS1)
DO1 <- DO %>% filter(STATION == 1)
DO1$OXYGEN <- as.numeric(DO1$OXYGEN)
DO1$DEPTH <- as.numeric(DO1$DEPTH)

# Find low DO data
DO2 <- DO1 %>% filter(OXYGEN < 2)

# Use only Aug and Sep data
a = ymd(DO2$Date)  
DO2$YEAR <- year(a)
DO2$MONTH <- month(a)  
DO2 <- DO2 %>% filter(MONTH == 8 | MONTH == 9 )

# In a given year, find min depth where DO is low
anox <- DO2 %>% group_by(YEAR) %>% summarize(zmin = min(DEPTH))
anox10 <- anox %>% filter(YEAR >= 2015)

za <- anox$zmin
mk<-MannKendall(za)
Tmean <- mean(za)
Tmedian <- median(za)
Tmin<- min(za)
Tmax <- max(za)

ggplot(anox, aes(YEAR, zmin)) + 
  geom_point() +
  labs(y = "za (m)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(17, 0)) +
  labs(title = 'Togus-1 Anoxic Depth, Aug-Sep Min')
dev.print(png,"Togus zA.png", res = 300, width = 1000)

mk10<-MannKendall(anox10$zmin)
Tmean10 <- mean(anox10$zmin)
Tmedian10 <- median(anox10$zmin)
Tmin10 <- min(anox10$zmin)
Tmax10 <- max(anox10$zmin)

ggplot(anox10, aes(YEAR, zmin)) + 
  geom_point() +
  labs(y = "za (m)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(17, 0)) +
  labs(title = 'Togus-1 Anoxic Depth, Aug-Sep Min')
dev.print(png,"Togus zA 5.png", res = 300, width = 1000)
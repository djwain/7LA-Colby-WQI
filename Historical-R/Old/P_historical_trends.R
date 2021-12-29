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

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Phosphorus.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 5274
P <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
PEC <- P %>% filter(Qualifier == 'EC')

# Make a month column
a <- ymd(PEC$Date)
PEC$Month <- month(a)
PEC$Year <- year(a)

# Find number of samples in each month
PEC %>% group_by(Month) %>% summarize(npts = n())

# Start with just August P
PEC <- PEC %>% filter(Month == 8 | Month == 7  | Month == 9)

# Average data from the same month in the same year
PECavg <- aggregate(`Total P`~Year, mean, data=PEC)
PECavg %>% group_by(Month) %>% summarize(npts = n())
PECavg10 <- PECavg %>% filter(Year >= 2011)

TP <- PECavg$`Total P`
mk<-MannKendall(TP)

tpp <- ggplot(PECavg, aes(Year,`Total P`)) + 
  geom_point() +
  labs(y = "P (ppb)", x = "Year") +
  scale_y_continuous(lim = c(0,24.5)) +
  labs(title = 'Average Great Pond Total Phosphorus')
tpp <- tpp + geom_hline(yintercept = 20, color = "red")
tpp + geom_hline(yintercept = 4.5, color = "green")

TP <- PECavg10$`Total P`
mk<-MannKendall(TP)

tpp10 <- ggplot(PECavg10, aes(Year,`Total P`)) + 
  geom_point() +
  labs(y = "P (ppb)", x = "Year") +
  scale_y_continuous(lim = c(0,24.5)) +
  labs(title = 'Average Great Pond Total Phosphorus over the Past 10 Years')
tpp10 <- tpp10 + geom_hline(yintercept = 20, color = "red")
tpp10 + geom_hline(yintercept = 4.5, color = "green")


ggplot(PECavg10, aes(Year,`Total P`)) + 
  geom_point() +
  labs(y = "P (ppb)", x = "Year") +
  #geom_smooth(method='loess') +
  labs(title = 'GP1+2 Epicore, Aug Avg, tau = 0.20, p = 0.65')

# Find bottom grab data
PBG <- P %>% filter(Qualifier == 'BG')

# Make a month column
a <- ymd(PBG$Date)
PBG$Month <- month(a)
PBG$Year <- year(a)

# Find number of samples in each month
PBG %>% group_by(Month) %>% summarize(npts = n())

# Start with just August P
PBG <- PBG %>% filter(Month == 9)

# Average data from the same month in the same year
PBGavg <- aggregate(`Total P`~Year+Month, mean, data=PBG)
PBGavg %>% group_by(Month) %>% summarize(npts = n())
PBGavg10 <- PBGavg %>% filter(Year >= 2010)

TP <- PBGavg$`Total P`
mk<-MannKendall(TP)

ggplot(PBGavg, aes(Year,`Total P`)) + 
  geom_point() +
  labs(y = "P (ppb)", x = "Year") +
  geom_smooth(method='loess') +
  labs(title = 'GP1+2 Bottom Grab, Aug Avg, tau = -0.33, p = 0.048')

TP <- PBGavg10$`Total P`
mk<-MannKendall(TP)

ggplot(PBGavg10, aes(Year,`Total P`)) + 
  geom_point() +
  labs(y = "P (ppb)", x = "Year") +
  #geom_smooth(method='loess') +
  labs(title = 'GP1+2 Bottom Grab, Aug Avg, tau = 0.24, p = 0.55')


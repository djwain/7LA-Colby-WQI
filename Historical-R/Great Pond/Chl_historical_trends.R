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
filename <- paste(filepath,"MaineLakes_Chlorophyll_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 5274
Chl <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
ChlC <- Chl %>% filter(`Sample Type` == 'C')

# Make a month column
a <- ymd(ChlC$Date)
ChlC$Month <- month(a)
ChlC$Year <- year(a)

# Find number of samples in each month
ChlC %>% group_by(Month) %>% summarize(npts = n())

# Start with just August Chl
ChlC <- ChlC %>% filter(Month == 8 | Month == 7  | Month == 9)

# Average data from the same month in the same year
ChlCavg <- aggregate(CHLA~Year, mean, data=ChlC)
ChlCavg %>% group_by(Month) %>% summarize(npts = n())
ChlCavg10 <- ChlCavg %>% filter(Year >= 2011)

CHLA <- ChlCavg$CHLA
mk<-MannKendall(CHLA)

tch <- ggplot(ChlCavg, aes(Year,CHLA)) + 
  geom_point() +
  labs(y = "Chl-a (ppb)", x = "Year") +
  scale_y_continuous(lim = c(0,8.5)) +
  labs(title = 'Average Great Pond Chlorophyll-a')
tch <- tch + geom_hline(yintercept = 7, color = "red")
tch + geom_hline(yintercept = 1.5, color = "green")

ggplot(ChlCavg, aes(Year,CHLA)) + 
  geom_point() +
  labs(y = "Chl-a (ppb)", x = "Year") +
  #geom_smooth(method='loess') +
  labs(title = 'GP1+2 Epicore, Aug Avg, tau = 0.14, p = 0.47')

CHLA <- ChlCavg10$CHLA
mk<-MannKendall(CHLA)

tch10 <- ggplot(ChlCavg10, aes(Year,CHLA)) + 
  geom_point() +
  labs(y = "Chl-a (ppb)", x = "Year") +
  scale_y_continuous(lim = c(0,8.5)) +
  labs(title = 'Average Great Pond Chlorophyll-a for the Past 10 Years')
tch10 <- tch10 + geom_hline(yintercept = 7, color = "red")
tch10 + geom_hline(yintercept = 1.5, color = "green")

ggplot(ChlCavg10, aes(Year,CHLA)) + 
  geom_point() +
  labs(y = "Chl-a (ppb)", x = "Year") +
  #geom_smooth(method='loess') +
  labs(title = 'GP1+2 Epicore, Aug Avg, tau = 0.41, p = 0.34')



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
filename <- paste(filepath,"MaineLakes_pHColorCond_Alk_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Cross Lake'
MIDAS1 <- 1674
pH <- dat %>% filter(MIDAS == MIDAS1)
pH1 <- pH %>% filter(Station == 1)

# Make a month pHumn
a <- ymd(pH1$Date)
pH1$Month <- month(a)
pH1$Year <- year(a)

# Find number of samples in each month
pH1 %>% group_by(Month) %>% summarize(npts = n())

# Start with just August pH
pH1 <- pH1 %>% filter(Month >= 5 | Month <= 10 ) %>% filter(pH_Method == "E" & Type == 'C')


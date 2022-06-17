# Creates box plots by decade using all the data

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load P data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Temp_DO.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Togus Pond'
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
DO2 <- DO2 %>% filter(MONTH >= 5 | MONTH <= 10 )

# In a given year, find min depth where DO is low
anox <- DO2 %>% group_by(YEAR) %>% summarize(zmin = min(DEPTH))


anox$Dec <- as.character(floor(anox$YEAR/10)*10)
anox$Dec <- paste(anox$Dec,'s',sep = "")

png(file="TP1_anox_box.png", units="in",width=5, height=5.5, res=400)

# Make box plot by year and by site
p <- ggplot(data = anox, aes(x = Dec,y = zmin)) +
  geom_boxplot() +
  labs(title = "Togus Pond Minimum Anoxic Depth by Decade", y = 'zmin (m)',x = '')
print(p)
dev.off()


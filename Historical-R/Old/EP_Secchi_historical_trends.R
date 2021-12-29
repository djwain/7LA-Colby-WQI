# This script scrapes the DEP data for secchi disk readings, combines it with the Colby data
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
MIDAS1 <- 5349
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

# Load data from Colby
lake <- 'East Pond'
site <- 'EPDEP1'
years <- 2015:2020
filepathC <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/Transparency/")
filename1 <- paste(filepathC,site,sep=""," - Secchi 2015-2020.xlsx")
dat1 <- ""
for(i in 1:length(years)){
  temp <- read_xlsx(filename1, sheet = as.character(years[i]))
  temp$Date <- as.character(temp$Date)
  dat1 <- rbind(dat1,temp)
}
dat1 <- dat1[-1,]
dat1$STATION <- 1



Colby <- dat1
Colby <- Colby %>% rename('DATE'='Date','SECCHI DEPTH'='Depth(m)')

LSM <- select(SDT_LSM,'DATE','SECCHI DEPTH','STATION')

Secchi <- rbind.data.frame(LSM,Colby)

a = ymd_hms(Secchi$DATE)  
Secchi$YEAR <- year(a)
Secchi$MONTH <- month(a)  

Secchi %>% group_by(MONTH) %>% summarize(npts = n())

# Use June-Sept
S <- Secchi %>% filter(MONTH >= 5 & MONTH <= 10 & STATION == 1)
S <- na.omit(S)
S <- distinct(S)
S$`SECCHI DEPTH` <- as.numeric(S$`SECCHI DEPTH`)

# Average data from the same month in the same year
Savg <- aggregate(`SECCHI DEPTH`~YEAR+MONTH, mean, data=S)
Smin <- S %>% group_by(YEAR) %>% summarise(yrmin = min(`SECCHI DEPTH`))

# Compute year average
Syr <- S %>% group_by(YEAR) %>% summarise(yrmean = mean(`SECCHI DEPTH`))
Syr$ft <- Syr$yrmean*3.28


mk<-MannKendall(Syr$yrmean)


ggplot(Syr, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(title = "Secchi Depth", y = "SDT (m)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(7.5,0)) +
  labs(title = 'Average East Pond Secchi')
 # labs(title = 'GP1 Secchi, May-Oct Avg, tau = -0.18, p = 0.083')
dev.print(png,paste(filepath,"EP Avg Secchi.png"), res = 300, width = 1000)

S10 <- filter(Syr,YEAR >= 2011)
mk<-MannKendall(S10$yrmean)

ggplot(S10, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(title = "Secchi Depth", y = "zS (m)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(7.5,0)) +
  labs(title = 'Average East Pond Secchi')
dev.print(png,paste(filepath,"EP Avg Secchi 10yr.png"), res = 300, width = 1000)

mk<-MannKendall(Smin$yrmin)


ggplot(Smin, aes(YEAR, yrmin)) + 
  geom_point() +
  labs(title = "Secchi Depth", y = "SDT (m)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(7.5,0)) +
  labs(title = 'Minimum East Pond Secchi')
  #labs(title = 'GP1 Secchi, May-Oct Min, tau = -0.32, p = 0.0017')
dev.print(png,paste(filepath,"EP Min Secchi.png"), res = 300, width = 1000)

S10 <- filter(Smin,YEAR >= 2011)
mk<-MannKendall(S10$yrmin)

ggplot(S10, aes(YEAR, yrmin)) + 
  geom_point()+
  labs(title = "Secchi Depth", y = "zS (m)", x = "Year") +
  #geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(7.5,0)) +
  labs(title = 'Minimum East Pond Secchi')
dev.print(png,paste(filepath,"EP Min Secchi 10yr.png"), res = 300, width = 1000)




# This script scrapes the DEP data for secchi disk readings
# and does a trend analysis on the entire dataset and the last 10 years following the DEP
# code per JD
# DJW 20MAY20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)
library(ggplot2)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Secchi_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 5272
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

# Load data from Colby
lake <- 'Long Pond'
site <- 'LPDEP1'
years <- 2015:2021
filepathC <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/Transparency/")
filename1 <- paste(filepathC,site,sep=""," - Secchi 2015-2021.xlsx")
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
Secchi$DAY <- day(a)  
Secchi$YDAY <- yday(Secchi$DATE)


# Use April 15-Oct 15
S <- Secchi %>% filter(YDAY >= 105 & YDAY <= 288)
S <- na.omit(S)
S <- distinct(S)
S$`SECCHI DEPTH` <- as.numeric(S$`SECCHI DEPTH`)

# Average data from the same day
Savgd <- aggregate(`SECCHI DEPTH`~YEAR+MONTH+DAY+STATION, mean, data=S)


# Compute year median by station
Syr1 <- Savgd %>% filter(STATION == 1) %>% group_by(YEAR) %>% summarize(zs10 = as.numeric(quantile(`SECCHI DEPTH`, c(0.10), na.rm = TRUE)))


# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Syr1$zs10)

sdp <- ggplot(Syr1, aes(YEAR, zs10)) + 
  geom_point() +
  labs(y = "SDT (m)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)) +
  labs(title = 'LPDEP1 10%-ile Secchi ')
sdp <- sdp + geom_hline(yintercept = 13.1, color = "red")
sdp + geom_hline(yintercept = 26.2, color = "green")
#labs(title = 'GP1 Secchi, May-Oct Avg, tau = -0.23, p = 0.020') 
dev.print(png,paste(filepathC,"LP1 10thpc Secchi.png"),width = 682, res = 150)


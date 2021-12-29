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
library(ggimage)


# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Secchi_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 5274
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

# Load data from Colby
lake <- 'Great Pond'
site <- 'GPDEP1'
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

lake <- 'Great Pond'
site <- 'GPDEP2'
years <- 2015:2020
filepathC <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/Transparency/")
filename1 <- paste(filepathC,site,sep=""," - Secchi 2015-2020.xlsx")
dat2 <- ""
for(i in 1:length(years)){
  temp <- read_xlsx(filename1, sheet = as.character(years[i]))
  temp$Date <- as.character(temp$Date)
  dat2 <- rbind(dat2,temp)
}
dat2 <- dat2[-1,]
dat2$STATION <- 2

Colby <- rbind(dat1,dat2)
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

sdp <- ggplot(Syr, aes(YEAR, ft)) + 
  geom_point() +
  labs(y = "SDT (ft)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)*3.28) +
  labs(title = 'Average Great Pond Secchi Disk Transparency')
sdp <- sdp + geom_hline(yintercept = 13.1, color = "red")
sdp + geom_hline(yintercept = 26.2, color = "green")
 #labs(title = 'GP1 Secchi, May-Oct Avg, tau = -0.23, p = 0.020') 
dev.print(png,paste(filepath,"GP1 Avg Secchi.png"),width = 682, res = 150)


S10 <- filter(Syr,YEAR >= 2011)
mk<-MannKendall(S10$yrmean)



S40 <- filter(Syr,YEAR < 2011)

ggplot()+
  geom_point(data = S10, aes(x = YEAR,y = ft)) +
  geom_hline(yintercept = 13.1, color = "red") +
  geom_hline(yintercept = 26.2, color = "green") +
  scale_x_continuous(breaks = seq(2011,2020,1)) +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)*3.28) +
  labs(title = "Water Clarity", y = "How deep we can see (ft)", x = "Year") +
  theme(text = element_text(size=20)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

  
  

dev.print(png,paste(filepath,"GP1 Avg Secchi 10y.png"),width = 682, res = 150)

ggplot(S10, aes(YEAR, yrmean)) + 
  geom_point() +
  labs(title = "Secchi Depth", y = "zS (m)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)) +
  labs(title = 'GP1 Secchi, May-Oct Avg, tau = -0.64, p = 0.01')
dev.print(png,paste(filepath,"GP1 Avg Secchi 10y.png"),width = 682, res = 150)

mk<-MannKendall(Smin$yrmin)


ggplot(Smin, aes(YEAR, yrmin)) + 
  geom_point() +
  labs(title = "Secchi Depth", y = "SDT (m)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)) +
  #labs(title = 'Minimum Great Pond Secchi')
  labs(title = 'GP1 Secchi, May-Oct Min, tau = -0.42, p = 0.00003')
dev.print(png,paste(filepath,"GP Min Secchi.png"), width = 682, res = 150)

S10 <- filter(Smin,YEAR >= 2011)
mk<-MannKendall(S10$yrmin)

ggplot(S10, aes(YEAR, yrmin)) + 
  geom_point() +
  labs(title = "Secchi Depth", y = "zS (m)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)) +
  labs(title = 'GP1 Secchi, May-Oct Min, tau = -0.64, p = 0.01')
dev.print(png,paste(filepath,"GP Min Secchi 10y.png"), width = 682, res = 150)


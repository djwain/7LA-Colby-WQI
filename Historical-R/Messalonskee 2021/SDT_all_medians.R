# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Secchi_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 5348
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

# Load data from Colby
lake <- 'McGrath'
site <- 'MPDEP1'
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

# Use June-Sept because they have an order of magnitude more measurements than other months
S <- Secchi #%>% filter(MONTH >= 5 & MONTH <= 10)
S <- na.omit(S)
S <- distinct(S)
S$`SECCHI DEPTH` <- 3.28*as.numeric(S$`SECCHI DEPTH`)

# Average data from the same month in the same year
Savg <- aggregate(`SECCHI DEPTH`~YEAR+MONTH+STATION, mean, data=S)

# Compute year average by station
Syr1 <- Savg %>% filter(STATION == 1) %>% group_by(YEAR) %>% summarise(yrmean = mean(`SECCHI DEPTH`))

# Plot data and trendline for each
yhigh<-max(S$`SECCHI DEPTH`)

splot <- Syr1


png(file="MP1_SDT_all.png", units="in",width=10, height=5, res=400)



plot(year(S$DATE),S$`SECCHI DEPTH`,ylim=c(yhigh,0),pch=21,col = 'azure3',bg = 'azure3',
     ylab="Water Clarity (ft)",xlab="Year")  
points(Syr1$YEAR,Syr1$yrmean,ylim=c(yhigh,0),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="Water Clarity (ft)",xlab="Year")
lines(lowess(Syr1$YEAR,Syr1$yrmean),col=4)
dev.off()


# This script scrapes the DEP data for secchi disk readings
# and does a trend analysis on the entire dataset and the last 10 years following the DEP
# code per JD
# DJW 20MAY20

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
MIDAS1 <- 5272
station <- 2
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1 & STATION == station)

# Load data from Colby
lake <- 'Long Pond'
site <- 'LPDEP2'
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
dat1$STATION <- 2

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
#S <- Secchi %>% filter(YDAY >= 105 & YDAY <= 288)
S <- na.omit(Secchi)
S <- distinct(S)
S$`SECCHI DEPTH` <- as.numeric(S$`SECCHI DEPTH`)

# Average data from the same day
Savgd <- aggregate(`SECCHI DEPTH`~YEAR+MONTH+DAY, mean, data=S)

# Average data from the same month in the same year
Savgm <- aggregate(`SECCHI DEPTH`~YEAR+MONTH, mean, data=Savgd)
Savg <- aggregate(`SECCHI DEPTH`~YEAR, mean, data=Savgm)


# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Savg$`SECCHI DEPTH`)

# Plot data and trendline for each
yhigh<-max(Savg$`SECCHI DEPTH`) +2

splot <- Savg
st <- '2'
mk <- mk1
png(file="LP2_SDT.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(Savg$`SECCHI DEPTH`),1)
smean <- round(mean(Savg$`SECCHI DEPTH`),1)
smed <-round(median(Savg$`SECCHI DEPTH`),1)
smax <- round(max(Savg$`SECCHI DEPTH`),1)

plot(Savg$YEAR,Savg$`SECCHI DEPTH`, las=1, ylim=c(0,yhigh),ylab="Average SDT (m)",xlab="Year")  
lines(lowess(Savg$YEAR,Savg$`SECCHI DEPTH`),col=4)
titles<-lake
titles2<-MIDAS1
titles3<-st
mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), side=3, line=1.9, font=2)
mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()


# Do Mann Kendall trend analysis on whole time series
Savg10 <- Savg %>% filter(YEAR > 2011)
mk10<-MannKendall(Savg10$`SECCHI DEPTH`)

# Plot data and trendline for each
yhigh<-max(Savg10$`SECCHI DEPTH`) +2

splot <- Savg10
st <- '2'
mk <- mk10
png(file="LP2_SDT_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(Savg10$`SECCHI DEPTH`),1)
smean <- round(mean(Savg10$`SECCHI DEPTH`),1)
smed <-round(median(Savg10$`SECCHI DEPTH`),1)
smax <- round(max(Savg10$`SECCHI DEPTH`),1)

plot(Savg10$YEAR,Savg10$`SECCHI DEPTH`, las=1, ylim=c(0,yhigh),ylab="Average SDT (m)",xlab="Year")  
lines(lowess(Savg10$YEAR,Savg10$`SECCHI DEPTH`),col=4)
titles<-lake
titles2<-MIDAS1
titles3<-st
mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), side=3, line=1.9, font=2)
mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()

yhigh<-max(Savgd$`SECCHI DEPTH`) +1
png(file="LP2_SDT_all.png", units="in",width=10, height=5, res=400)
plot(Savgd$YEAR,Savgd$`SECCHI DEPTH`,ylim=c(yhigh,0),pch=21,col = 'azure3',bg = 'azure3',
     ylab="SDT (m)",xlab="Year",title('Long Pond Station 2'))  
points(Savg$YEAR,Savg$`SECCHI DEPTH`,ylim=c(yhigh,0),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="SDT (m)",xlab="Year")
#lines(lowess(Savg$YEAR,Savg$`SECCHI DEPTH`),col=4)
dev.off()

yhigh<-max(Savgd$`SECCHI DEPTH`) +1
png(file="LP2_SDT_all_ft.png", units="in",width=5, height=5, res=400)
plot(Savgd$YEAR,3.28*Savgd$`SECCHI DEPTH`,ylim=c(3.28*yhigh,0),pch=21,col = 'azure3',bg = 'azure3',
     ylab="Water Clarity (ft)",xlab="Year",title('Long Pond South Basin'))  
points(Savg$YEAR,3.28*Savg$`SECCHI DEPTH`,ylim=c(3.28*yhigh,0),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="Water Clarity (ft)",xlab="Year")
lines(lowess(Savg$YEAR,3.28*Savg$`SECCHI DEPTH`),col=4)
dev.off()



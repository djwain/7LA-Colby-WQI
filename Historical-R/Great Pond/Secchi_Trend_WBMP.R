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

# Use June-Sept because they have an order of magnitude more measurements than other months
S <- Secchi %>% filter(MONTH >= 5 & MONTH <= 10)
S <- na.omit(S)
S <- distinct(S)
S$`SECCHI DEPTH` <- as.numeric(S$`SECCHI DEPTH`)

# Average data from the same month in the same year
Savg <- aggregate(`SECCHI DEPTH`~YEAR+MONTH+STATION, mean, data=S)

# Compute year average by station
Syr1 <- Savg %>% filter(STATION == 1) %>% group_by(YEAR) %>% summarise(yrmean = mean(`SECCHI DEPTH`))
Syr2 <- Savg %>% filter(STATION == 2) %>% group_by(YEAR) %>% summarise(yrmean = mean(`SECCHI DEPTH`))

# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Syr1$yrmean)
mk2<-MannKendall(Syr2$yrmean)

# Do Mann Kendall trend analysis on last 10 years
Syr1_10 <- Syr1 %>% filter(YEAR >= 2011)
Syr2_10 <- Syr2 %>% filter(YEAR >= 2011)
mk1_10 <- MannKendall(Syr1_10$yrmean)
mk2_10 <- MannKendall(Syr2_10$yrmean)

# Plot data and trendline for each
yhigh<-max(Savg$`SECCHI DEPTH`)+2

splot <- Syr1
st <- '1'
mk <- mk1
png(file="GP1_SDT_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$YEAR,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average Jun-Sep SDT (m)",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrmean),col=4)
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

splot <- Syr1_10
st <- '1'
mk <- mk1_10
png(file="GP1_SDT_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$YEAR,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average Jun-Sep SDT (m)",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrmean),col=4)
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

splot <- Syr2
st <- '2'
mk <- mk2
png(file="GP2_SDT_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$YEAR,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average Jun-Sep SDT (m)",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrmean),col=4)
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

splot <- Syr2_10
st <- '2'
mk <- mk2_10
png(file="GP2_SDT_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$YEAR,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average Jun-Sep SDT (m)",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrmean),col=4)
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


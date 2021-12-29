# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Chlorophyll_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'McGrath'
MIDAS1 <- 5348
Chl <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
ChlC <- Chl %>% filter(`Sample Type` == 'C')

# Make a month column
a <- ymd(ChlC$Date)
ChlC$Month <- month(a)
ChlC$Year <- year(a)

npts <- ChlC %>% group_by(Month) %>% summarize(npts = n())

# Use May - October data
C <- ChlC %>% filter(Month >= 5 & Month <= 10)

# Average data from the same month in the same year
Cavg <- aggregate(CHLA~Year+Month+Station, mean, data=C)

# Compute year average by station
Cyr1 <- Cavg %>% filter(Station == 1) %>% group_by(Year) %>% summarise(yrmean = mean(CHLA))
Cyr2 <- Cavg %>% filter(Station == 2) %>% group_by(Year) %>% summarise(yrmean = mean(CHLA))

# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Cyr1$yrmean)
#mk2<-MannKendall(Cyr2$yrmean)

# Do Mann Kendall trend analysis on last 10 years
Cyr1_10 <- Cyr1 %>% filter(Year >= 2011)
Cyr2_10 <- Cyr2 %>% filter(Year >= 2011)
mk1_10 <- MannKendall(Cyr1_10$yrmean)
#mk2_10 <- MannKendall(Cyr2_10$yrmean)

# Plot data and trendline for each
yhigh<-max(Cyr1$yrmean)+2

splot <- Cyr1
st <- '1'
mk <- mk1
png(file="MP1_CHLA_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$Year,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average May-Oct CHLA (ppb)",xlab="Year")  
lines(lowess(splot$Year,splot$yrmean),col=4)
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

splot <- Cyr1_10
st <- '1'
mk <- mk1_10
png(file="MP1_CHLA_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$Year,splot$yrmean, las=1, ylim=c(0,8),ylab="Average May-Oct CHLA (ppb)",xlab="Year")  
lines(lowess(splot$Year,splot$yrmean),col=4)
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


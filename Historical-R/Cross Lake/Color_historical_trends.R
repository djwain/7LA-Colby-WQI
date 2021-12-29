# This script scrapes the DEP data for phosphorus measurements
# and does a trend analysis on the entire dataset and the last 10 years 
# DJW 20MAY20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_pHColorCond_Alk_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Cross Lake'
MIDAS1 <- 1674
Col <- dat %>% filter(MIDAS == MIDAS1)
Col1 <- Col %>% filter(Station == 1)

# Make a month column
a <- ymd(Col1$Date)
Col1$Month <- month(a)
Col1$Year <- year(a)

# Find number of samples in each month
Col1 %>% group_by(Month) %>% summarize(npts = n())

# Start with just August Chl
Col1 <- Col1 %>% filter(Month >= 5 | Month <= 10 ) %>% filter(Type == 'C')

# AVerage data from same day
Cday<- Col1 %>% group_by(Date) %>% summarise(daymean = mean(`Color (SPU)`, na.rm = TRUE))
a = ymd(Cday$Date)  
Cday$YEAR <- year(a)
Cday$MONTH <- month(a)  


# Compute year average by station
Cyr1 <- Cday %>% group_by(YEAR) %>% summarise(yrmean = mean(daymean, na.rm = TRUE))

# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Cyr1$yrmean)



# Plot data and trendline for each
yhigh<-max(Pavg$`Total P`)+3

splot <- Pyr1
st <- '1'
mk <- mk1
png(file="CL1_P_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$Year,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average May-Oct TP (ppb)",xlab="Year")  
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

splot <- Pyr1_10
st <- '1'
mk <- mk1_10
png(file="CL1_P_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$Year,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average May-Oct TP (ppb)",xlab="Year")  
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
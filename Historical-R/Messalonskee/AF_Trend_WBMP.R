# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load P data from LSM
dat <- read_excel("AFvsYR.xlsx", sheet = 1)

# MIDAS is identifier for LSM data
lake <- 'Togus Pond'
MIDAS1 <- 9931

# In a given year, find min depth where DO is low
mk1<-MannKendall(dat$`Anoxic Factor (days)`)

# Plot data and trendline for each
yhigh<-max(dat$`Anoxic Factor (days)`)+2

splot <- dat
st <- '1'
mk <- mk1
png(file="TP1_AF_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$`Anoxic Factor (days)`),1)
smean <- round(mean(splot$`Anoxic Factor (days)`),1)
smed <-round(median(splot$`Anoxic Factor (days)`),1)
smax <- round(max(splot$`Anoxic Factor (days)`),1)

plot(splot$Year,splot$`Anoxic Factor (days)`, las=1, ylim=c(0,yhigh),ylab="Anoxic Factor (days)",xlab="Year")  
lines(lowess(splot$Year,splot$`Anoxic Factor (days)`),col=4)
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


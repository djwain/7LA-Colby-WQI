# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Set wd
setwd("~/Documents/GitHub/7LA-Colby-WQI/Historical-R/Unity")

# Load P data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Temp_DO.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Unity Pond'
site <- 'UP1'
MIDAS1 <- 5172
st <- 1
yr10 <- 2012

DO <- dat %>% filter(MIDAS == MIDAS1 & Station == st)
DO1 <- DO
DO1$OXYGEN <- as.numeric(DO1$OXYGEN)
DO1$DEPTH <- as.numeric(DO1$DEPTH)

# Find low DO data
DO2 <- DO1 %>% filter(OXYGEN < 2)

# Use only Aug and Sep data
a = ymd(DO2$Date)  
DO2$YEAR <- year(a)
DO2$MONTH <- month(a)  
DO2$YDAY <- yday(DO2$Date)
DO2 <- DO2 %>% filter(MONTH >= 4 & MONTH <= 10)

# In a given year, find min depth where DO is low
anox1 <- DO2 %>% group_by(YEAR) %>% summarize(zmin = min(DEPTH))

mk1<-MannKendall(anox1$zmin)

# Plot data and trendline for each
yhigh<-max(anox1$zmin)

splot <- anox1
mk <- mk1
png(file=paste(site,"_MAD.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$zmin),1)
smean <- round(mean(splot$zmin),1)
smed <-round(median(splot$zmin),1)
smax <- round(max(splot$zmin),1)

plot(splot$Year,splot$zmin, las=1, ylim=c(0,yhigh),ylab="Minimum Anoxic Depth (m)",xlab="Year")  
lines(lowess(splot$Year,splot$zmin),col=4)
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
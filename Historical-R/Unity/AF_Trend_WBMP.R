# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filename <- "UP1 Anoxic Factor.xlsx"
dat <- read_excel(filename, col_names = FALSE)
colnames(dat) <- c("Year","AF")

# MIDAS is identifier for LSM data
lake <- 'Unity Pond'
site <- 'UP1'
MIDAS1 <- 5172
st <- 1
yr10 <- 2012


# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(dat$AF)

# Plot data and trendline for each
yhigh<-max(dat$AF)

splot <- dat
mk <- mk1
png(file=paste(site, "_AF.png", sep = ""), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$AF),1)
smean <- round(mean(splot$AF),1)
smed <-round(median(splot$AF),1)
smax <- round(max(splot$AF),1)

plot(splot$Year,splot$AF, las=1, ylim=c(0,yhigh),ylab="Anoxic Factor",xlab="Year")  
lines(lowess(splot$Year,splot$AF),col=4)
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




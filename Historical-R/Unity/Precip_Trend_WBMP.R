# Trend analysis for rain and snow data
# DJW 1/31/21

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Set wd
setwd("~/Documents/GitHub/7LA-Colby-WQI/Historical-R/Unity")


# Load weather data
filename <- "WatervillePrecip.csv"
dat <- read.csv(filename,header = TRUE, stringsAsFactors = FALSE)



mkp<-MannKendall(dat$Total.PRCP)
mks<-MannKendall(dat$Total.SNOW)

# Plot data and trendline for each
yhighp<-max(dat$Total.PRCP)+6
yhigh<-yhighp

splot <- dat
mk <- mkp
png(file=paste("Waterville_precip_1896-2021.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$Total.PRCP),1)
smean <- round(mean(splot$Total.PRCP),1)
smed <-round(median(splot$Total.PRCP),1)
smax <- round(max(splot$Total.PRCP),1)

plot(splot$Year,splot$Total.PRCP, las=1, ylim=c(0,yhigh),ylab="Total Annual Precipitation",xlab="Year")  
lines(lowess(splot$Year,splot$Total.PRCP),col=4)
mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()

# Plot data and trendline for each
yhighs<-max(dat$Total.SNOW)+7
yhigh<-yhighs

splot <- dat
mk <- mks
png(file=paste("Waterville_snow_1896-2021.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$Total.SNOW),1)
smean <- round(mean(splot$Total.SNOW),1)
smed <-round(median(splot$Total.SNOW),1)
smax <- round(max(splot$Total.SNOW),1)

plot(splot$Year,splot$Total.SNOW, las=1, ylim=c(0,yhigh),ylab="Total Annual Snow",xlab="Year")  
lines(lowess(splot$Year,splot$Total.SNOW),col=4)
mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()

dat <- dat %>% filter(Year >= 1977)

mkp<-MannKendall(dat$Total.PRCP)
mks<-MannKendall(dat$Total.SNOW)

# Plot data and trendline for each
yhigh<-yhighp

splot <- dat
mk <- mkp
png(file=paste("Waterville_precip_1977-2021.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$Total.PRCP),1)
smean <- round(mean(splot$Total.PRCP),1)
smed <-round(median(splot$Total.PRCP),1)
smax <- round(max(splot$Total.PRCP),1)

plot(splot$Year,splot$Total.PRCP, las=1, ylim=c(0,yhigh),ylab="Total Annual Precipitation",xlab="Year")  
lines(lowess(splot$Year,splot$Total.PRCP),col=4)
mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()

# Plot data and trendline for each
yhigh<-yhighs

splot <- dat
mk <- mks
png(file=paste("Waterville_snow_1977-2021.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$Total.SNOW),1)
smean <- round(mean(splot$Total.SNOW),1)
smed <-round(median(splot$Total.SNOW),1)
smax <- round(max(splot$Total.SNOW),1)

plot(splot$Year,splot$Total.SNOW, las=1, ylim=c(0,yhigh),ylab="Total Annual Snow",xlab="Year")  
lines(lowess(splot$Year,splot$Total.SNOW),col=4)
mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()

dat <- dat %>% filter(Year >= 2012)

mkp<-MannKendall(dat$Total.PRCP)
mks<-MannKendall(dat$Total.SNOW)

# Plot data and trendline for each
yhigh<-yhighp

splot <- dat
mk <- mkp
png(file=paste("Waterville_precip_2012-2021.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$Total.PRCP),1)
smean <- round(mean(splot$Total.PRCP),1)
smed <-round(median(splot$Total.PRCP),1)
smax <- round(max(splot$Total.PRCP),1)

plot(splot$Year,splot$Total.PRCP, las=1, ylim=c(0,yhigh),ylab="Total Annual Precipitation",xlab="Year")  
lines(lowess(splot$Year,splot$Total.PRCP),col=4)
mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()

# Plot data and trendline for each
yhigh<-yhighs

splot <- dat
mk <- mks
png(file=paste("Waterville_snow_2012-2021.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$Total.SNOW),1)
smean <- round(mean(splot$Total.SNOW),1)
smed <-round(median(splot$Total.SNOW),1)
smax <- round(max(splot$Total.SNOW),1)

plot(splot$Year,splot$Total.SNOW, las=1, ylim=c(0,yhigh),ylab="Total Annual Snow",xlab="Year")  
lines(lowess(splot$Year,splot$Total.SNOW),col=4)
mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()
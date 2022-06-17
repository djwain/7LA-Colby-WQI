# Trend analysis for rain and snow data
# DJW 1/31/21

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load weather data
filename <- "Fort Kent Precip 1980-2020 Aroostook County, ME Daily Summary.csv"
dat <- read.csv(filename,header = TRUE, stringsAsFactors = FALSE)

dat$DATE <- as.Date(dat$DATE,"%m/%d/%Y")
dat$YEAR <- year(dat$DATE)

prcp_yr <- dat %>% filter(YEAR <= 2020) %>% group_by(YEAR) %>% summarise(yrtot = sum(PRCP, na.rm = TRUE))
snow_yr <- dat  %>% filter(YEAR <= 2020) %>% group_by(YEAR) %>% summarise(yrtot = sum(SNOW, na.rm = TRUE))

mkp<-MannKendall(prcp_yr$yrtot)
mks<-MannKendall(snow_yr$yrtot)

# Plot precip data and trendline for each
yhigh<-max(prcp_yr$yrtot)*1.1

splot <- prcp_yr
mk <- mkp
png(file="FK_precip_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrtot),1)
smean <- round(mean(splot$yrtot),1)
smed <-round(median(splot$yrtot),1)
smax <- round(max(splot$yrtot),1)

plot(splot$YEAR,splot$yrtot, las=1, ylim=c(0,yhigh),ylab="Total Annual Precipitation",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrtot),col=4)

mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()

# Plot snow data and trendline for each
yhigh<-max(snow_yr$yrtot)*1.1

splot <- snow_yr
mk <- mks
png(file="FK_snow_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrtot),1)
smean <- round(mean(splot$yrtot),1)
smed <-round(median(splot$yrtot),1)
smax <- round(max(splot$yrtot),1)

plot(splot$YEAR,splot$yrtot, las=1, ylim=c(0,yhigh),ylab="Total Annual Snow",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrtot),col=4)

mtext(text=sprintf("tau=%1.3f",(mk$tau[1])),adj=0, side=3, line=0.6, cex=.8)
mtext(text=sprintf("p=%1.3f",(mk$sl[1])),adj=1, side=3,line=0.6, cex=.8)
mtext(text=sprintf(" min=%1.1f",smin),adj=0, side=3,line=-1, cex=.8)
mtext(text=sprintf(" mean=%1.1f",smean),adj=0, side=3,line=-2, cex=.8)
mtext(text=sprintf(" med=%1.1f",smed),adj=0, side=3,line=-3, cex=.8)
mtext(text=sprintf(" max=%1.1f",smax),adj=0, side=3,line=-4, cex=.8)
dev.off()



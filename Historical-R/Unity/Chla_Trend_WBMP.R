# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)
library(ggplot2)

# Set wd
setwd("~/Documents/GitHub/7LA-Colby-WQI/Historical-R/Unity")


# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Chlorophyll_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Unity Pond'
site <- 'UP1'
MIDAS1 <- 5172
st <- 1
yr10 <- 2012
Chl <- dat %>% filter(MIDAS == MIDAS1 & Station == st)

# Find epicore data
ChlC <- Chl %>% filter(`Sample Type` == 'C')

# Make a month column
a <- ymd(ChlC$Date)
ChlC$Month <- month(a)
ChlC$Year <- year(a)
ChlC$Day <- day(a)
ChlC$Yday <- yday(ChlC$Date)

# Filter data from April - October
ChlC <- ChlC %>% filter(Month >= 4 & Month <= 10)

# Average data from the same month in the same year
Cavgd <- aggregate(CHLA~Year+Month+Day, mean, data=ChlC)

# Average data from the same month in the same year
Cavgm <- aggregate(CHLA~Year+Month, mean, data=Cavgd)
Cavg <- aggregate(CHLA~Year, mean, data=Cavgm)

# Compute median by decade
Cavgd$Dec <- as.character(floor(Cavgd$Year/10)*10)
Cavgd$Dec <- paste(Cavgd$Dec,'s',sep = "")

# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Cavg$CHLA)

# Plot data and trendline for each
yhigh<-max(Cavg$CHLA)

splot <- Cavg
mk <- mk1
png(file=paste(site, "_Chl.png",sep = ""), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$CHLA),1)
smean <- round(mean(splot$CHLA),1)
smed <-round(median(splot$CHLA),1)
smax <- round(max(splot$CHLA),1)

plot(splot$Year,splot$CHLA, las=1, ylim=c(0,yhigh),ylab="Average Chl-a (ppb)",xlab="Year")  
lines(lowess(splot$Year,splot$CHLA),col=4)
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
Cavg10 <- Cavg %>% filter(Year >= yr10)
mk10<-MannKendall(Cavg10$CHLA)

# Plot data and trendline for each
yhigh<-max(Cavg10$CHLA)+6

splot <- Cavg10
mk <- mk10
png(file=paste(site, "_Chl10.png",sep = ""), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$CHLA),1)
smean <- round(mean(splot$CHLA),1)
smed <-round(median(splot$CHLA),1)
smax <- round(max(splot$CHLA),1)

plot(splot$Year,splot$CHLA, las=1, ylim=c(0,yhigh),ylab="Average Chl-a (ppb)",xlab="Year")  
lines(lowess(splot$Year,splot$CHLA),col=4)
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

# Plot all Chl data with average on top
yhigh<-max(Cavgd$CHLA)
png(file=paste(site, "_Chl_all.png",sep = ""), units="in",width=10, height=5, res=400)
plot(Cavgd$Year,Cavgd$CHLA,ylim=c(0,yhigh),pch=21,col = 'azure3',bg = 'azure3',
     ylab="Chl-a (ppb)",xlab="Year")
mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), side=3, line=1.9, font=2)
points(Cavg$Year,Cavg$CHLA,ylim=c(0,yhigh),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="Chl-a (ppb)",xlab="Year")
dev.off()


# Plot Chl box plots by decade
png(file=paste(site, "_Chl_box.png",sep = ""), units="in",width=5, height=5.5, res=400)
S1 <- Cavgd
p <- ggplot(data = S1, aes(x = Dec,y = CHLA)) +
  geom_boxplot() +
  labs(title = paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), y = "Chl-a (ppb)",x = '')
print(p)
dev.off()


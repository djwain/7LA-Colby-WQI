# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Phosphorus.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'McGrath Pond'
MIDAS1 <- 5348
Pall <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
PC <- Pall %>% filter(`Sample Type` == 'C')

# Make a month column
a <- ymd(PC$Date)
PC$Month <- month(a)
PC$Year <- year(a)
PC$Day <- day(a)
PC$Yday <- yday(PC$Date)

# Filter for Apr. 15 - Oct. 15
P <- PC %>% filter(Yday >= 105 & Yday <= 288 & Year >=2011)

# Average data from the date
Pavgd <- aggregate(`Total P`~Year+Month+Day+Station, mean, data=P)

# Compute year median by station
Pyr1 <- Pavgd %>% group_by(Year) %>% summarise(yrmed = median(`Total P`))

# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Pyr1$yrmed)

# Plot data and trendline for each
yhigh<-max(Pyr1$yrmed)+1

splot <- Pyr1
st <- '1'
mk <- mk1
png(file="MP1_P_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmed),1)
smean <- round(mean(splot$yrmed),1)
smed <-round(median(splot$yrmed),1)
smax <- round(max(splot$yrmed),1)

plot(splot$Year,splot$yrmed, las=1, ylim=c(0,yhigh),ylab="Median Apr. 15 - Oct. 15 TP - EC (ppb)",xlab="Year")  
lines(lowess(splot$Year,splot$yrmed),col=4)
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


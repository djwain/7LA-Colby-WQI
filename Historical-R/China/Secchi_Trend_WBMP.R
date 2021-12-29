# This script scrapes the DEP data for secchi disk readings
# and does a trend analysis on the entire dataset and the last 10 years following the DEP
# code per JD
# DJW 20MAY20

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
lake <- 'China Lake'
MIDAS1 <- 5448
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

Secchi<- select(SDT_LSM,'DATE','SECCHI DEPTH','STATION')

a = ymd(Secchi$DATE)  
Secchi$YEAR <- year(a)
Secchi$MONTH <- month(a)  
Secchi$DAY <- day(a)  
Secchi$YDAY <- yday(Secchi$DATE)

npts <- Secchi %>% group_by(MONTH) %>% summarize(npts = n())

# Use April 15-Oct 15?
S <- Secchi %>% filter(YDAY >= 105 & YDAY <= 288)
S <- na.omit(S)
S <- distinct(S)
S$`SECCHI DEPTH` <- as.numeric(S$`SECCHI DEPTH`)

# Average data from the same day
Savgd <- aggregate(`SECCHI DEPTH`~YEAR+MONTH+DAY+STATION, mean, data=S)


# Compute year average by station
Syr1 <- Savgd %>% filter(STATION == 1) %>% group_by(YEAR) %>% summarise(yrmed = median(`SECCHI DEPTH`))

# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Syr1$yrmed)

# Plot data and trendline for each
yhigh<-max(Syr1$yrmed)+2

splot <- Syr1
st <- '1'
mk <- mk1
png(file="China1_SDT_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmed),1)
smean <- round(mean(splot$yrmed),1)
smed <-round(median(splot$yrmed),1)
smax <- round(max(splot$yrmed),1)

plot(splot$YEAR,splot$yrmed, las=1, ylim=c(0,yhigh),ylab="Median Apr. 15 - Oct. 15 SDT (m)",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrmed),col=4)
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

# Compute year average by station
Syr2 <- Savgd %>% filter(STATION == 2) %>% group_by(YEAR) %>% summarise(yrmed = median(`SECCHI DEPTH`))

# Do Mann Kendall trend analysis on past 10 years
mk1<-MannKendall(Syr2$yrmed)

# Plot data and trendline for each
yhigh<-max(Syr2$yrmed)+2

splot <- Syr2
st <- '2'
mk <- mk1
png(file="China2_SDT_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmed),1)
smean <- round(mean(splot$yrmed),1)
smed <-round(median(splot$yrmed),1)
smax <- round(max(splot$yrmed),1)

plot(splot$YEAR,splot$yrmed, las=1, ylim=c(0,yhigh),ylab="Median Apr. 15 - Oct. 15 SDT (m)",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrmed),col=4)
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

# Compute year average by station
Syr3 <- Savgd %>% filter(STATION == 3) %>% group_by(YEAR) %>% summarise(yrmed = median(`SECCHI DEPTH`))

# Do Mann Kendall trend analysis on past 10 years
mk1<-MannKendall(Syr3$yrmed)

# Plot data and trendline for each
yhigh<-max(Syr3$yrmed)+2

splot <- Syr3
st <- '3'
mk <- mk1
png(file="China3_SDT_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmed),1)
smean <- round(mean(splot$yrmed),1)
smed <-round(median(splot$yrmed),1)
smax <- round(max(splot$yrmed),1)

plot(splot$YEAR,splot$yrmed, las=1, ylim=c(0,yhigh),ylab="Median Apr. 15 - Oct. 15 SDT (m)",xlab="Year")  
lines(lowess(splot$YEAR,splot$yrmed),col=4)
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

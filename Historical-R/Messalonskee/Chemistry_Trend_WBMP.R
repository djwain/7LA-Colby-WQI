# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

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
lake <- 'McGrath Pond'
MIDAS1 <- 5348
Chem <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
ChemC <-Chem %>% filter(`Type` == 'C')

# Make a month column
a <- ymd(ChemC$Date)
ChemC$Month <- month(a)
ChemC$Year <- year(a)
ChemC$Day <- day(a)
ChemC$Yday <- yday(ChemC$Date)

# Use Apr 15-OCt 15
C <- ChemC %>% filter(Yday >= 105 & Yday <= 288)

# Average data from the same month in the same year
pHd <- aggregate(pH~Year+Month+Day+Station+pH_Method, mean, data=C)
Colord <- aggregate(`Color (SPU)`~Year+Month+Day+Station+AORT, mean, data=C)
Condd <- aggregate(`Conductivity (uS)`~Year+Month+Day+Station+Cond_Method, mean, data=C)
Alkd <- aggregate(`Alkalinity (mg/L)`~Year+Month+Day+Station, mean, data=C)

# pH Analysis
pHyr1 <- pHd %>% filter(Station == 1 & pH_Method == 'E') %>% group_by(Year) %>% summarise(yrmed = median(pH))

# Color Analysis
Colyr1 <- Colord %>% filter(Station == 1 & AORT == 'T') %>% group_by(Year) %>% summarise(yrmed = median(`Color (SPU)`))

# Conductivity Analysis
Condyr1 <- Condd %>% filter(Station == 1 & Cond_Method != 'E') %>% group_by(Year) %>% summarise(yrmed = median(`Conductivity (uS)`))

# Alkalinity Analysis
Alkyr1 <- Alkd %>% filter(Station == 1) %>% group_by(Year) %>% summarise(yrmed = median(`Alkalinity (mg/L)`))

# CONDUCTIVITY
mk1<-MannKendall(Condyr1$yrmed)

# Plot data and trendline for each
yhigh<-max(Condyr1$yrmed)+20

splot <- Condyr1
st <- '1'
mk <- mk1
png(file="MP1_Cond_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmed),1)
smean <- round(mean(splot$yrmed),1)
smed <-round(median(splot$yrmed),1)
smax <- round(max(splot$yrmed),1)

plot(splot$Year,splot$yrmed, las=1, ylim=c(0,yhigh),ylab="Median Apr. 15 - Oct. 15 Conductivity (uS)",xlab="Year")  
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

# Alkalinity Analysis
mk1<-MannKendall(Alkyr1$yrmed)

# Plot data and trendline for each
yhigh<-max(Alkyr1$yrmed)+1

splot <- Alkyr1
st <- '1'
mk <- mk1
png(file="MP1_Alk_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmed),1)
smean <- round(mean(splot$yrmed),1)
smed <-round(median(splot$yrmed),1)
smax <- round(max(splot$yrmed),1)

plot(splot$Year,splot$yrmed, las=1, ylim=c(0,yhigh),ylab="Median Apr. 15 - Oct. 15 Alkalinity (mg/L)",xlab="Year")  
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

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
lake <- 'China Lake'
MIDAS1 <- 5448
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



# pH <- aggregate(pH~Year+Month+Station+pH_Method, mean, data=pHd)
# Color <- aggregate(`Color (SPU)`~Year+Month+Station+AORT, mean, data=Colord)
# Cond <- aggregate(`Conductivity (uS)`~Year+Month+Station, mean, data=Condd)
# Alk <- aggregate(`Alkalinity (mg/L)`~Year+Month+Station, mean, data=Alkd)

# pH Analysis
pHyr1 <- pHd %>% filter(Station == 1 & pH_Method == 'E') %>% group_by(Year) %>% summarise(yrmed = median(pH))
pHyr2 <- pHd %>% filter(Station == 2 & pH_Method == 'E') %>% group_by(Year) %>% summarise(yrmed = median(pH))
pHyr3 <- pHd %>% filter(Station == 3 & pH_Method == 'E') %>% group_by(Year) %>% summarise(yrmed = median(pH))

# Color Analysis
Colyr1 <- Colord %>% filter(Station == 1 & AORT == 'T') %>% group_by(Year) %>% summarise(yrmed = median(`Color (SPU)`))
Colyr2 <- Colord %>% filter(Station == 2 & AORT == 'T') %>% group_by(Year) %>% summarise(yrmed = median(`Color (SPU)`))
Colyr3 <- Colord %>% filter(Station == 3 & AORT == 'T') %>% group_by(Year) %>% summarise(yrmed = median(`Color (SPU)`))

# Conductivity Analysis
Condyr1 <- Condd %>% filter(Station == 1 & Cond_Method == 'F') %>% group_by(Year) %>% summarise(yrmed = median(`Conductivity (uS)`))
Condyr2 <- Condd %>% filter(Station == 2 & Cond_Method == 'F') %>% group_by(Year) %>% summarise(yrmed = median(`Conductivity (uS)`))
Condyr3 <- Condd %>% filter(Station == 3 & Cond_Method == 'F') %>% group_by(Year) %>% summarise(yrmed = median(`Conductivity (uS)`))

# Alkalinity Analysis
Alkyr1 <- Alkd %>% filter(Station == 1) %>% group_by(Year) %>% summarise(yrmed = median(`Alkalinity (mg/L)`))
Alkyr2 <- Alkd %>% filter(Station == 2) %>% group_by(Year) %>% summarise(yrmed = median(`Alkalinity (mg/L)`))
Alkyr3 <- Alkd %>% filter(Station == 3) %>% group_by(Year) %>% summarise(yrmed = median(`Alkalinity (mg/L)`))

# CONDUCTIVITY
mk1<-MannKendall(Condyr3$yrmed)

# Plot data and trendline for each
yhigh<-max(Condyr3$yrmed)+20

splot <- Condyr3
st <- '3'
mk <- mk1
png(file="China3_Cond_all.png", units="in",width=5, height=5.5, res=400)

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
png(file="China1_Alk_all.png", units="in",width=5, height=5.5, res=400)

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

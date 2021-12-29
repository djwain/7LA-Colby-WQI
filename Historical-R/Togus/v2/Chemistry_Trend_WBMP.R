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
lake <- 'Togus Pond'
MIDAS1 <- 9931
Chem <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
ChemC <-Chem %>% filter(`Type` == 'C')

# Make a month column
a <- ymd(ChemC$Date)
ChemC$Month <- month(a)
ChemC$Year <- year(a)
ChemC$Day <- day(a)

# Use May - October data
C <- ChemC %>% filter(Month >= 5 & Month <= 10)

# Average data from the same month in the same year
pHd <- aggregate(pH~Year+Month+Day+Station+pH_Method, mean, data=C)
Colord <- aggregate(`Color (SPU)`~Year+Month+Day+Station+AORT, mean, data=C)
Condd <- aggregate(`Conductivity (uS)`~Year+Month+Day+Station, mean, data=C)
Alkd <- aggregate(`Alkalinity (mg/L)`~Year+Month+Day+Station, mean, data=C)



pH <- aggregate(pH~Year+Month+Station+pH_Method, mean, data=pHd)
Color <- aggregate(`Color (SPU)`~Year+Month+Station+AORT, mean, data=Colord)
Cond <- aggregate(`Conductivity (uS)`~Year+Month+Station, mean, data=Condd)
Alk <- aggregate(`Alkalinity (mg/L)`~Year+Month+Station, mean, data=Alkd)

# pH Analysis
pHyr1 <- pH %>% filter(Station == 1 & pH_Method == 'E') %>% group_by(Year) %>% summarise(yrmean = mean(pH))

# Color Analysis
Colyr1 <- Color %>% filter(Station == 1 & AORT == 'T') %>% group_by(Year) %>% summarise(yrmean = mean(`Color (SPU)`))

# Conductivity Analysis
Condyr1 <- Cond %>% filter(Station == 1) %>% group_by(Year) %>% summarise(yrmean = mean(`Conductivity (uS)`))
mk1<-MannKendall(Condyr1$yrmean)

# Plot data and trendline for each
yhigh<-max(Cond$`Conductivity (uS)`)+2

splot <- Condyr1
st <- '1'
mk <- mk1
png(file="TP1_Cond_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$Year,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average May-Oct Conductivity (uS)",xlab="Year")  
lines(lowess(splot$Year,splot$yrmean),col=4)
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
Alkyr1 <- Alk %>% filter(Station == 1) %>% group_by(Year) %>% summarise(yrmean = mean(`Alkalinity (mg/L)`))
mk1<-MannKendall(Alkyr1$yrmean)

# Plot data and trendline for each
yhigh<-max(Alk$`Alkalinity (mg/L)`)+ 2

splot <- Alkyr1
st <- '1'
mk <- mk1
png(file="TP1_Alk_all.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$yrmean),1)
smean <- round(mean(splot$yrmean),1)
smed <-round(median(splot$yrmean),1)
smax <- round(max(splot$yrmean),1)

plot(splot$Year,splot$yrmean, las=1, ylim=c(0,yhigh),ylab="Average May-Oct Alkalinity (mg/L)",xlab="Year")  
lines(lowess(splot$Year,splot$yrmean),col=4)
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

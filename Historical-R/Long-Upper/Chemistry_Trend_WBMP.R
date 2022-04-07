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
lake <- 'Long Pond'
MIDAS1 <- 5272
Chem <- dat %>% filter(MIDAS == MIDAS1 & Station == 1)

# Find epicore data
ChemC <-Chem %>% filter(`Type` == 'C')

# Make a month column
a <- ymd(ChemC$Date)
ChemC$Month <- month(a)
ChemC$Year <- year(a)
ChemC$Day <- day(a)
ChemC$Yday <- yday(ChemC$Date)

# Use Apr 15-OCt 15
C <- ChemC #%>% filter(Yday >= 105 & Yday <= 288)

# Average data from the same month in the same year
pHd <- aggregate(pH~Year+Month+Day+pH_Method, mean, data=C)
Colord <- aggregate(`Color (SPU)`~Year+Month+Day+AORT, mean, data=C)
Condd <- aggregate(`Conductivity (uS)`~Year+Month+Day+Cond_Method, mean, data=C)
Alkd <- aggregate(`Alkalinity (mg/L)`~Year+Month+Day, mean, data=C)

pHm <- aggregate(pH~Year+Month+pH_Method, mean, data=pHd)
Colorm <- aggregate(`Color (SPU)`~Year+Month+AORT, mean, data=Colord)
Condm <- aggregate(`Conductivity (uS)`~Year+Month+Cond_Method, mean, data=Condd)
Alkm <- aggregate(`Alkalinity (mg/L)`~Year+Month, mean, data=Alkd)

pHyr <- aggregate(pH~Year+Month+pH_Method, mean, data=pHm)
Colyr <- aggregate(`Color (SPU)`~Year+Month+AORT, mean, data=Colorm)
Condyr <- aggregate(`Conductivity (uS)`~Year+Month+Cond_Method, mean, data=Condm)
Alkyr <- aggregate(`Alkalinity (mg/L)`~Year+Month, mean, data=Alkm)

pHyr1 <- pHyr %>% filter(pH_Method == 'G') 
Colyr1 <- Colyr %>% filter(AORT == 'T')
Condyr1 <- Condyr %>% filter(Cond_Method == 'L')
Alkyr1 <- Alkyr

# pH


# Color
mk1 <- MannKendall(Colyr1$`Color (SPU)`)

# Plot data and trendline for each
yhigh<-max(Colyr1$`Color (SPU)`)+1
  

splot <- Colyr1
st <- '1'
mk <- mk1
png(file="LP1_Col.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$`Color (SPU)`),1)
smean <- round(mean(splot$`Color (SPU)`),1)
smed <-round(median(splot$`Color (SPU)`),1)
smax <- round(max(splot$`Color (SPU)`),1)

plot(splot$Year,splot$`Color (SPU)`, las=1, ylim=c(0,yhigh),ylab="Average Color (SPU)",xlab="Year")  
lines(lowess(splot$Year,splot$`Color (SPU)`),col=4)
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
Colavg10 <- Colyr1 %>% filter(Year > 2011)
mk10<-MannKendall(Colavg10$`Color (SPU)`)

# Plot data and trendline for each
yhigh<-max(Colavg10$`Color (SPU)`)+1

splot <- Colavg10
st <- '1'
mk <- mk10
png(file="LP1_Col_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$`Color (SPU)`),1)
smean <- round(mean(splot$`Color (SPU)`),1)
smed <-round(median(splot$`Color (SPU)`),1)
smax <- round(max(splot$`Color (SPU)`),1)

plot(splot$Year,splot$`Color (SPU)`, las=1, ylim=c(0,yhigh),ylab="Average Color (SPU)",xlab="Year")  
lines(lowess(splot$Year,splot$`Color (SPU)`),col=4)
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

yhigh<-max(Colyr1$`Color (SPU)`)+1
png(file="LP1_Col_all.png", units="in",width=10, height=5, res=400)
plot(Colord$Year,Colord$`Color (SPU)`,ylim=c(0,yhigh),pch=21,col = 'azure3',bg = 'azure3',
     ylab="Color (SPU)",xlab="Year",title('Long Pond Station 1'))  
points(Colyr1$Year,Colyr1$`Color (SPU)`,ylim=c(0,yhigh),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="Color (SPU)",xlab="Year")
dev.off()

Colord$Dec <- as.character(floor(Colord$Year/10)*10)
Colord$Dec <- paste(Pavgd$Dec,'s',sep = "")

png(file="LP1_Col_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Colord
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Color (SPU)`)) +
  geom_boxplot() +
  labs(title = "Long Pond St. 1 Color by Decade", y = 'Color (SPU)',x = '')
print(p)
dev.off()

# Conductivity
mk1 <- MannKendall(Condyr1$`Conductivity (uS)`)

# Plot data and trendline for each
yhigh<-max(Condyr1$`Conductivity (uS)`)+4


splot <- Condyr1
st <- '1'
mk <- mk1
png(file="LP1_Cond.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$`Conductivity (uS)`),1)
smean <- round(mean(splot$`Conductivity (uS)`),1)
smed <-round(median(splot$`Conductivity (uS)`),1)
smax <- round(max(splot$`Conductivity (uS)`),1)

plot(splot$Year,splot$`Conductivity (uS)`, las=1, ylim=c(0,yhigh),ylab="Average Conductivity (uS)",xlab="Year")  
lines(lowess(splot$Year,splot$`Conductivity (uS)`),col=4)
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


Condd$Dec <- as.character(floor(Condd$Year/10)*10)
Condd$Dec <- paste(Condd$Dec,'s',sep = "")

png(file="LP1_Cond_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Condd
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Conductivity (uS)`)) +
  geom_boxplot() +
  labs(title = "Long Pond St. 1 Conductivity by Decade", y = 'Conductivity (uS)',x = '')
print(p)
dev.off()

# Alkalinity
mk1 <- MannKendall(Alkyr1$`Alkalinity (mg/L)`)

# Plot data and trendline for each
yhigh<-max(Alkyr1$`Alkalinity (mg/L)`)


splot <- Alkyr1
st <- '1'
mk <- mk1
png(file="LP1_Alk.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$`Alkalinity (mg/L)`),1)
smean <- round(mean(splot$`Alkalinity (mg/L)`),1)
smed <-round(median(splot$`Alkalinity (mg/L)`),1)
smax <- round(max(splot$`Alkalinity (mg/L)`),1)

plot(splot$Year,splot$`Alkalinity (mg/L)`, las=1, ylim=c(0,yhigh),ylab="Average Alkalinity (mg/L)",xlab="Year")  
lines(lowess(splot$Year,splot$`Alkalinity (mg/L)`),col=4)
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


Alkd$Dec <- as.character(floor(Alkd$Year/10)*10)
Alkd$Dec <- paste(Alkd$Dec,'s',sep = "")

png(file="LP1_Alk_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Alkd
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Alkalinity (mg/L)`)) +
  geom_boxplot() +
  labs(title = "Long Pond St. 1 Alkalinity by Decade", y = 'Alkalinity (mg/L)',x = '')
print(p)
dev.off()
# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)
library(gg)

# Set wd
setwd("~/Documents/GitHub/7LA-Colby-WQI/Historical-R/Unity")


# Load secchi data from LSM
filepath <- "~/Documents/GitHub/7LA-Colby-WQI/Historical-R/Maine Lakes/"
filename <- paste(filepath,"MaineLakes_pHColorCond_Alk_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Unity Pond'
site <- 'UP1'
MIDAS1 <- 5172
st <- 1
yr10 <- 2012
Chem <- dat %>% filter(MIDAS == MIDAS1 & Station == st)

# Find epicore data
ChemC <-Chem %>% filter(`Type` == 'C')

# Make a month column
a <- ymd(ChemC$Date)
ChemC$Month <- month(a)
ChemC$Year <- year(a)
ChemC$Day <- day(a)
ChemC$Yday <- yday(ChemC$Date)

# Filter data from April - October
C <- ChemC %>% filter(Month >= 4 & Month <= 10)

# Average data from the same month in the same year
pHd <- aggregate(pH~Year+Month+Day+pH_Method, mean, data=C)
pHd <- pHd %>% filter(pH_Method != 'C')
Colord <- aggregate(`Color (SPU)`~Year+Month+Day+AORT, mean, data=C)
Colord <- Colord %>% filter(AORT == 'T')
Condd <- aggregate(`Conductivity (uS)`~Year+Month+Day, mean, data=C)
Alkd <- aggregate(`Alkalinity (mg/L)`~Year+Month+Day+Alk_Method, mean, data=C)
Alkd <- Alkd %>% filter(Alk_Method != 'M')

pHm <- aggregate(pH~Year+Month, mean, data=pHd)
Colorm <- aggregate(`Color (SPU)`~Year+Month, mean, data=Colord)
Condm <- aggregate(`Conductivity (uS)`~Year+Month, mean, data=Condd)
Alkm <- aggregate(`Alkalinity (mg/L)`~Year+Month, mean, data=Alkd)

pHyr <- aggregate(pH~Year, mean, data=pHm)
Colyr <- aggregate(`Color (SPU)`~Year, mean, data=Colorm)
Condyr <- aggregate(`Conductivity (uS)`~Year, mean, data=Condm)
Alkyr <- aggregate(`Alkalinity (mg/L)`~Year, mean, data=Alkm)

pHyr1 <- pHyr 
Colyr1 <- Colyr 
Condyr1 <- Condyr 
Alkyr1 <- Alkyr



# pH
mk1 <- MannKendall(pHyr1$pH)

# Plot data and trendline for each
yhigh<-max(pHyr1$pH)+2


splot <- pHyr1
mk <- mk1
png(file=paste(site,"_pH.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$pH),1)
smean <- round(mean(splot$pH),1)
smed <-round(median(splot$pH),1)
smax <- round(max(splot$pH),1)

plot(splot$Year,splot$pH, las=1, ylim=c(0,yhigh),ylab="Average pH",xlab="Year")  
lines(lowess(splot$Year,splot$pH),col=4)
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
pHyr10 <- pHyr1 %>% filter(Year >= yr10)
mk10<-MannKendall(pHyr10$pH)

# Plot data and trendline for each
yhigh<-max(pHyr10$pH)+2

splot <- pHyr10
mk <- mk10
png(file=paste(site, "_pH10.png"), units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$pH),1)
smean <- round(mean(splot$pH),1)
smed <-round(median(splot$pH),1)
smax <- round(max(splot$pH),1)

plot(splot$Year,splot$pH, las=1, ylim=c(0,yhigh),ylab="Average pH",xlab="Year")  
lines(lowess(splot$Year,splot$pH),col=4)
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

yhigh<-max(pHd$pH)
png(file=paste(site,"_pH_all.png"), units="in",width=10, height=5, res=400)
plot(pHd$Year,pHd$pH,ylim=c(0,yhigh),pch=21,col = 'azure3',bg = 'azure3',
     ylab="pH",xlab="Year")  
mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), side=3, line=1.9, font=2)
points(pHyr1$Year,pHyr1$pH,ylim=c(0,yhigh),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="pH",xlab="Year")
dev.off()

# Medians by decade
pHd$Dec <- as.character(floor(pHd$Year/10)*10)
pHd$Dec <- paste(pHd$Dec,'s',sep = "")

# Make box plot by year and by site
png(file=paste(site,"_pH_box.png"), units="in",width=5, height=5.5, res=400)
S1 <- pHd
p <- ggplot(data = S1, aes(x = Dec,y = pH)) +
  geom_boxplot() +
  labs(title = paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), y = "pH",x = '')
print(p)
dev.off()

# Color
mk1 <- MannKendall(Colyr1$`Color (SPU)`)

# Plot data and trendline for each
yhigh<-max(Colyr1$`Color (SPU)`)+2
  

splot <- Colyr1
mk <- mk1
png(file=paste(site,"_Col.png"), units="in",width=5, height=5.5, res=400)

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
Colavg10 <- Colyr1 %>% filter(Year >= yr10)
mk10<-MannKendall(Colavg10$`Color (SPU)`)

# Plot data and trendline for each
yhigh<-max(Colavg10$`Color (SPU)`)+1

splot <- Colavg10
mk <- mk10
png(file=paste(site, "_Col_10.png"), units="in",width=5, height=5.5, res=400)

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

yhigh<-max(Colord$`Color (SPU)`)+1
png(file=paste(site,"_Col_all.png"), units="in",width=10, height=5, res=400)
plot(Colord$Year,Colord$`Color (SPU)`,ylim=c(0,yhigh),pch=21,col = 'azure3',bg = 'azure3',
     ylab="Color (SPU)",xlab="Year")  
mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), side=3, line=1.9, font=2)
points(Colyr1$Year,Colyr1$`Color (SPU)`,ylim=c(0,yhigh),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="Color (SPU)",xlab="Year")
dev.off()

# Medians by decade
Colord$Dec <- as.character(floor(Colord$Year/10)*10)
Colord$Dec <- paste(Colord$Dec,'s',sep = "")

# Make box plot by year and by site
png(file=paste(site,"_Col_box.png"), units="in",width=5, height=5.5, res=400)
S1 <- Colord
p <- ggplot(data = S1, aes(x = Dec,y = `Color (SPU)`)) +
  geom_boxplot() +
  labs(title = paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), y = "Color (SPU)",x = '')
print(p)
dev.off()

# Conductivity
mk1 <- MannKendall(Condyr1$`Conductivity (uS)`)

# Plot data and trendline for each
yhigh<-max(Condyr1$`Conductivity (uS)`)+4


splot <- Condyr1
mk <- mk1
png(file=paste(site,"_Cond.png"), units="in",width=5, height=5.5, res=400)

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

# Do Mann Kendall trend analysis on past 10 years
Condyr10 <- Condyr1 %>% filter(Year >= yr10)
mk10 <- MannKendall(Condyr10$`Conductivity (uS)`)

# Plot data and trendline for each
yhigh<-max(Condyr10$`Conductivity (uS)`)+20

splot <- Condyr10
mk <- mk10
png(file=paste(site,"_Cond10.png"), units="in",width=5, height=5.5, res=400)

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

yhigh<-max(Condd$`Conductivity (uS)`)+1
png(file=paste(site,"_Cond_all.png"), units="in",width=10, height=5, res=400)
plot(Condd$Year,Condd$`Conductivity (uS)`,ylim=c(0,yhigh),pch=21,col = 'azure3',bg = 'azure3',
     ylab="Conductivity (uS)",xlab="Year")  
mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), side=3, line=1.9, font=2)
points(Condyr1$Year,Condyr1$`Conductivity (uS)`,ylim=c(0,yhigh),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="Conductivity (uS)",xlab="Year")
dev.off()

Condd$Dec <- as.character(floor(Condd$Year/10)*10)
Condd$Dec <- paste(Condd$Dec,'s',sep = "")

# Make box plot by year and by site
png(file=paste(site,"_Cond_box.png"), units="in",width=5, height=5.5, res=400)
S1 <- Condd
p <- ggplot(data = S1, aes(x = Dec,y = `Conductivity (uS)`)) +
  geom_boxplot() +
  labs(title = paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), y = "Conductivity (uS)",x = '')
print(p)
dev.off()

# Alkalinity
mk1 <- MannKendall(Alkyr1$`Alkalinity (mg/L)`)

# Plot data and trendline for each
yhigh<-max(Alkyr1$`Alkalinity (mg/L)`)+4


splot <- Alkyr1
mk <- mk1
png(file=paste(site,"_Alk.png"), units="in",width=5, height=5.5, res=400)

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

# Do Mann Kendall trend analysis on past 10 years
Alkyr10 <- Alkyr1 %>% filter(Year >= yr10)
# Alkalinity
mk10 <- MannKendall(Alkyr10$`Alkalinity (mg/L)`)

# Plot data and trendline for each
yhigh<-max(Alkyr10$`Alkalinity (mg/L)`)+6


splot <- Alkyr10
mk <- mk10
png(file=paste(site,"_Alk10.png"), units="in",width=5, height=5.5, res=400)

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

yhigh<-max(Alkd$`Alkalinity (mg/L)`)
png(file=paste(site,"_Alk_all.png"), units="in",width=10, height=5, res=400)
plot(Alkd$Year,Alkd$`Alkalinity (mg/L)`,ylim=c(0,yhigh),pch=21,col = 'azure3',bg = 'azure3',
     ylab="Alkalinity (mg/L)",xlab="Year")  
mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), side=3, line=1.9, font=2)
points(Alkyr1$Year,Alkyr1$`Alkalinity (mg/L)`,ylim=c(0,yhigh),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="Alkalinity (mg/L)",xlab="Year")
dev.off()

Alkd$Dec <- as.character(floor(Alkd$Year/10)*10)
Alkd$Dec <- paste(Alkd$Dec,'s',sep = "")

# Make box plot by year and by site
png(file=paste(site,"_Alk_box.png"), units="in",width=5, height=5.5, res=400)
S1 <- Alkd
p <- ggplot(data = S1, aes(x = Dec,y = `Alkalinity (mg/L)`)) +
  geom_boxplot() +
  labs(title = paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), y = "Alkalinity (mg/L)",x = '')
print(p)
dev.off()
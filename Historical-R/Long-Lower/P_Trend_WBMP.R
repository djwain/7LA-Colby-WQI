# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)
library(ggplot2)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Phosphorus.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Long Pond'
MIDAS1 <- 5272
Pall <- dat %>% filter(MIDAS == MIDAS1 & Station == 2)

# Find epicore data
PC <- Pall %>% filter(`Sample Type` == 'C')

# Make a month column
a <- ymd(PC$Date)
PC$Month <- month(a)
PC$Year <- year(a)
PC$Day <- day(a)
PC$Yday <- yday(PC$Date)



# Average data from the date
Pavgd <- aggregate(`Total P`~Year+Month+Day, mean, data=PC)

# Average data from the same month in the same year
Pavgm <- aggregate(`Total P`~Year+Month, mean, data=Pavgd)

# Get rid of May data
Pavgm2 <- Pavgm %>% filter(Pavgm$Month > 5)
Pavg <- aggregate(`Total P`~Year, mean, data=Pavgm2)


# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Pavg$`Total P`)

# Plot data and trendline for each
yhigh<-max(Pavg$`Total P`)+3

splot <- Pavg
st <- '2'
mk <- mk1
png(file="LP2_P.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$`Total P`),1)
smean <- round(mean(splot$`Total P`),1)
smed <-round(median(splot$`Total P`),1)
smax <- round(max(splot$`Total P`),1)

plot(splot$Year,splot$`Total P`, las=1, ylim=c(0,yhigh),ylab="Average TP - EC (ppb)",xlab="Year")  
lines(lowess(splot$Year,splot$`Total P`),col=4)
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
Pavg10 <- Pavg %>% filter(Year > 2011)
mk10<-MannKendall(Pavg10$`Total P`)

# Plot data and trendline for each
yhigh<-max(Pavg10$`Total P`)+3

splot <- Pavg10
st <- '1'
mk <- mk10
png(file="LP2_P_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$`Total P`),1)
smean <- round(mean(splot$`Total P`),1)
smed <-round(median(splot$`Total P`),1)
smax <- round(max(splot$`Total P`),1)

plot(splot$Year,splot$`Total P`, las=1, ylim=c(0,yhigh),ylab="Average TP - EC (ppb)",xlab="Year")  
lines(lowess(splot$Year,splot$`Total P`),col=4)
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

yhigh<-max(Pavgd$`Total P`)+3
png(file="LP2_P_all.png", units="in",width=10, height=5, res=400)
plot(Pavgd$Year,Pavgd$`Total P`,ylim=c(0,yhigh),pch=21,col = 'azure3',bg = 'azure3',
     ylab="TP - EC (ppb)",xlab="Year",title('Long Pond Station 2'))  
points(Pavg$Year,Pavg$`Total P`,ylim=c(0,yhigh),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="TP - EC (ppb)",xlab="Year")
dev.off()

Pavgd$Dec <- as.character(floor(Pavgd$Year/10)*10)
Pavgd$Dec <- paste(Pavgd$Dec,'s',sep = "")

png(file="LP2_P_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Pavgd
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Total P`)) +
  geom_boxplot() +
  labs(title = "Long Pond St. 2 Total Phosphorus - EC by Decade", y = 'TP (ppb)',x = '')
print(p)
dev.off()


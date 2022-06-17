# This script scrapes the DEP data for secchi disk readings
# and does a trend analysis on the entire dataset and the last 10 years following the DEP
# code per JD
# DJW 20MAY20

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
filename <- paste(filepath,"MaineLakes_Secchi_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Unity Pond'
site <- 'UP1'
MIDAS1 <- 5172
st <- 1
yr10 <- 2012
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1 & STATION == st)

LSM <- select(SDT_LSM,'DATE','SECCHI DEPTH','STATION')

Secchi <- LSM


a = ymd(Secchi$DATE)  
Secchi$YEAR <- year(a)
Secchi$MONTH <- month(a)  
Secchi$DAY <- day(a)  
Secchi$YDAY <- yday(Secchi$DATE)

# Filter data from April - October
Secchi <- Secchi %>% filter(MONTH >= 4 & MONTH <= 10)


S <- na.omit(Secchi)
S <- distinct(S)
S$`SECCHI DEPTH` <- as.numeric(S$`SECCHI DEPTH`)

# Average data from the same day
Savgd <- aggregate(`SECCHI DEPTH`~YEAR+MONTH+DAY, mean, data=S)
outfilename <- paste(site,"_secchi.csv",sep = "")
Savgd2 <- Savgd
names(Savgd2) <- make.names(names(Savgd2))
write.csv(Savgd2, outfilename, row.names = FALSE)

# Average data from the same month in the same year
Savgm <- aggregate(`SECCHI DEPTH`~YEAR+MONTH, mean, data=Savgd)
Savg <- aggregate(`SECCHI DEPTH`~YEAR, mean, data=Savgm)

# Compute medians by decade
Savgd$Dec <- as.character(floor(Savgd$YEAR/10)*10)
Savgd$Dec <- paste(Savgd$Dec,'s',sep = "")
Sdec1 <- Savgd %>%  group_by(Dec) %>% summarise(decmed = median(`SECCHI DEPTH`))

# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Savg$`SECCHI DEPTH`)

# Plot data and trendline for each
yhigh<-max(Savg$`SECCHI DEPTH`) +2

splot <- Savg
mk <- mk1
png(file=paste(site, "_SDT.png",sep = ""), units="in",width=5, height=5.5, res=400)

smin <- round(min(Savg$`SECCHI DEPTH`),1)
smean <- round(mean(Savg$`SECCHI DEPTH`),1)
smed <-round(median(Savg$`SECCHI DEPTH`),1)
smax <- round(max(Savg$`SECCHI DEPTH`),1)

plot(Savg$YEAR,Savg$`SECCHI DEPTH`, las=1, ylim=c(0,yhigh),ylab="Average SDT (m)",xlab="Year")  
lines(lowess(Savg$YEAR,Savg$`SECCHI DEPTH`),col=4)
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
Savg10 <- Savg %>% filter(YEAR >= yr10)
mk10<-MannKendall(Savg10$`SECCHI DEPTH`)

# Plot data and trendline for each
yhigh<-max(Savg10$`SECCHI DEPTH`) +1

splot <- Savg10
mk <- mk10
png(file=paste(site, "_SDT10.png",sep = ""), units="in",width=5, height=5.5, res=400)

smin <- round(min(Savg10$`SECCHI DEPTH`),1)
smean <- round(mean(Savg10$`SECCHI DEPTH`),1)
smed <-round(median(Savg10$`SECCHI DEPTH`),1)
smax <- round(max(Savg10$`SECCHI DEPTH`),1)

plot(Savg10$YEAR,Savg10$`SECCHI DEPTH`, las=1, ylim=c(0,yhigh),ylab="Average SDT (m)",xlab="Year")  
lines(lowess(Savg10$YEAR,Savg10$`SECCHI DEPTH`),col=4)
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

# Plot all secchi data with average on top
yhigh<-max(Savgd$`SECCHI DEPTH`) +1
png(file=paste(site, "_SDT_all.png",sep = ""), units="in",width=10, height=5, res=400)
plot(Savgd$YEAR,Savgd$`SECCHI DEPTH`,ylim=c(yhigh,0),pch=21,col = 'azure3',bg = 'azure3',
     ylab="SDT (m)",xlab="Year")  
mtext(text=paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), side=3, line=1.9, font=2)
points(Savg$YEAR,Savg$`SECCHI DEPTH`,ylim=c(yhigh,0),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="SDT (m)",xlab="Year")
#lines(lowess(Savg$YEAR,Savg$`SECCHI DEPTH`),col=4)
dev.off()

# Plot secchi box plots by decade
png(file=paste(site, "_SDT_box.png",sep = ""), units="in",width=5, height=5.5, res=400)
S1 <- Savgd
p <- ggplot(data = S1, aes(x = Dec,y = `SECCHI DEPTH`)) +
  geom_boxplot() +
  labs(title = paste(titles, "\n", " (MIDAS ",titles2," - Station ",titles3,")",sep = ""), y = 'SDT (m)',x = '')
print(p)
dev.off()




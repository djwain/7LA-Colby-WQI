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

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Secchi_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 5274
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

# Load data from Colby
lake <- 'Great Pond'
site <- 'GPDEP1'
years <- 2015:2021
filepathC <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/Transparency/")
filename1 <- paste(filepathC,site,sep=""," - Secchi 2015-2021.xlsx")
dat1 <- ""
for(i in 1:length(years)){
  temp <- read_xlsx(filename1, sheet = as.character(years[i]))
  temp$Date <- as.character(temp$Date)
  dat1 <- rbind(dat1,temp)
}
dat1 <- dat1[-1,]
dat1$STATION <- 1

Colby <- dat1
Colby <- Colby %>% rename('DATE'='Date','SECCHI DEPTH'='Depth(m)')

LSM <- select(SDT_LSM,'DATE','SECCHI DEPTH','STATION')

Secchi <- rbind.data.frame(LSM,Colby)

a = ymd_hms(Secchi$DATE)  
Secchi$YEAR <- year(a)
Secchi$MONTH <- month(a)  
Secchi$DAY <- day(a)  
Secchi$YDAY <- yday(Secchi$DATE)


# Use April 15-Oct 15
S <- Secchi %>% filter(YDAY >= 105 & YDAY <= 288)
S <- na.omit(S)
S <- distinct(S)
S$`SECCHI DEPTH` <- as.numeric(S$`SECCHI DEPTH`)

# Average data from the same day
Savgd <- aggregate(`SECCHI DEPTH`~YEAR+MONTH+DAY+STATION, mean, data=S)


# Compute year median by station
Syr1 <- Savgd %>% filter(STATION == 1) %>% group_by(YEAR) %>% summarise(yrmed = median(`SECCHI DEPTH`))
Syr1$ft <- Syr1$yrmed*3.28

# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(Syr1$ft)

sdp <- ggplot(Syr1, aes(YEAR, ft)) + 
  geom_point() +
  labs(y = "SDT (ft)", x = "Year") +
  geom_smooth(method='loess') +
  scale_y_continuous(trans = 'reverse', lim = c(10,0)*3.28) +
  labs(title = 'Median Great Pond Secchi Disk Transparency')
sdp <- sdp + geom_hline(yintercept = 13.1, color = "red")
sdp + geom_hline(yintercept = 26.2, color = "green")
#labs(title = 'GP1 Secchi, May-Oct Avg, tau = -0.23, p = 0.020') 
#dev.print(png,paste(filepathC,"GP1 Median Secchi.png"),width = 682, res = 150)
ggsave(paste(filepathC,"GP1 Median Secchi.png"))
# Plot data and trendline for each
yhigh<-max(Syr1$yrmed)+2

splot <- Syr1
st <- '1'
mk <- mk1
png(file="GP1_SDT.png", units="in",width=5, height=5.5, res=400)

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

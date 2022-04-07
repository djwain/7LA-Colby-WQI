# This script plots the secchi trends following Deeds East Pond WBMP
# DJW 12/21/20

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filename <- "/Users/djw56/Dropbox (Personal)/My Mac (ens-help’s MacBook Pro)/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Long Pond/Profiles/LPDEP2 Anoxic Factor.xlsx"
dat <- read_excel(filename, col_names = FALSE)
colnames(dat) <- c("Year","AF")

lake <- "Long Pond"
MIDAS1 <-5272


# Do Mann Kendall trend analysis on whole time series
mk1<-MannKendall(dat$AF)

# Plot data and trendline for each
yhigh<-max(dat$AF)+1

splot <- dat
st <- '2'
mk <- mk1
png(file="LP2_AF.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$AF),1)
smean <- round(mean(splot$AF),1)
smed <-round(median(splot$AF),1)
smax <- round(max(splot$AF),1)

plot(splot$Year,splot$AF, las=1, ylim=c(0,yhigh),ylab="Anoxic Factor",xlab="Year")  
lines(lowess(splot$Year,splot$AF),col=4)
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
dat10 <- dat %>% filter(Year > 2011)
mk10<-MannKendall(dat10$AF)

# Plot data and trendline for each
yhigh<-max(dat10$AF)

splot <- dat10
st <- '1'
mk <- mk10
png(file="LP1_AF_10.png", units="in",width=5, height=5.5, res=400)

smin <- round(min(splot$AF),1)
smean <- round(mean(splot$AF),1)
smed <-round(median(splot$AF),1)
smax <- round(max(splot$AF),1)

plot(splot$Year,splot$AF, las=1, ylim=c(0,yhigh),ylab="Anoxic Factor",xlab="Year")  
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
png(file="LP1_PBG_all.png", units="in",width=10, height=5, res=400)
plot(Pavgd$Year,Pavgd$`Total P`,ylim=c(0,yhigh),pch=21,col = 'azure3',bg = 'azure3',
     ylab="TP - BG (ppb)",xlab="Year",title('Long Pond Station 1'))  
points(Pavg$Year,Pavg$`Total P`,ylim=c(0,yhigh),pch=21,cex = 1.5,bg = 'aquamarine3',ylab="TP - BG (ppb)",xlab="Year")
dev.off()

Pavgd$Dec <- as.character(floor(Pavgd$Year/10)*10)
Pavgd$Dec <- paste(Pavgd$Dec,'s',sep = "")

png(file="LP1_PBG_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Pavgd
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Total P`)) +
  geom_boxplot() +
  labs(title = "Long Pond St. 1 Total Phosphorus - BG by Decade", y = 'TP (ppb)',x = '')
print(p)
dev.off()


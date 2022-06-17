# Creates box plots by decade using all the data

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Secchi_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
MIDAS1 <- 5348
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

# Load data from Colby
lake <- 'McGrath'
site <- 'MPDEP1'
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
Secchi$Year <- year(a)
Secchi$Month <- month(a)  
Secchi$Day <- day(a)  
Secchi$Yday <- yday(Secchi$DATE)

S <- Secchi %>% filter(Yday >= 105 & Yday <= 288)
S <- na.omit(S)
S <- distinct(S)
S$SDT <- as.numeric(S$`SECCHI DEPTH`)

Savgd <- aggregate(SDT~Year+Month+Day+STATION, mean, data=S)

Savgd$Dec <- as.character(floor(Savgd$Year/10)*10)
Savgd$Dec <- paste(Savgd$Dec,'s',sep = "")

png(file="MP1_SDT_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Savgd %>% filter(STATION == 1)
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = SDT)) +
  geom_boxplot() +
  labs(title = "McGrath St. 1 Secchi Disk Transparency by Decade", y = 'SDT (m)',x = '')
print(p)
dev.off()

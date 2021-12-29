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
lake <- 'China Lake'
MIDAS1 <- 5448
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

Secchi<- select(SDT_LSM,'DATE','SECCHI DEPTH','STATION')

a = ymd(Secchi$DATE)  
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

png(file="China1_SDT_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Savgd %>% filter(STATION == 1)
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = SDT)) +
  geom_boxplot() +
  labs(title = "China Lake St. 1 Secchi Disk Transparency by Decade", y = 'SDT (m)',x = '')
print(p)
dev.off()

png(file="China2_SDT_box.png", units="in",width=5, height=5.5, res=400)

S2 <- Savgd %>% filter(STATION == 2)
# Make box plot by year and by site
p <- ggplot(data = S2, aes(x = Dec,y = SDT)) +
  geom_boxplot() +
  labs(title = "China Lake St. 2 Secchi Disk Transparency by Decade", y = 'SDT (m)',x = '')
print(p)
dev.off()

png(file="China3_SDT_box.png", units="in",width=5, height=5.5, res=400)

S3 <- Savgd %>% filter(STATION == 3)
# Make box plot by year and by site
p <- ggplot(data = S3, aes(x = Dec,y = SDT)) +
  geom_boxplot() +
  labs(title = "China Lake St. 3 Secchi Disk Transparency by Decade", y = 'SDT (m)',x = '')
print(p)
dev.off()

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
lake <- 'Togus Pond'
MIDAS1 <- 9931
SDT_LSM <- dat %>% filter(`Lake Code (MIDAS)` == MIDAS1)

Secchi<- select(SDT_LSM,'DATE','SECCHI DEPTH','STATION')

a = ymd(Secchi$DATE)  
Secchi$Year <- year(a)
Secchi$Month <- month(a)  
Secchi$Dec <- as.character(floor(Secchi$Year/10)*10)
Secchi$Dec <- paste(Secchi$Dec,'s',sep = "")

# Use MAy-Oct because they have an order of magnitude more measurements than other months
S <- Secchi %>% filter(Month >= 5 & Month <= 10) %>% filter(STATION ==1)
S <- distinct(S)
S$SDT <- as.numeric(S$`SECCHI DEPTH`)

png(file="TP1_SDT_box.png", units="in",width=5, height=5.5, res=400)

# Make box plot by year and by site
p <- ggplot(data = S, aes(x = Dec,y = SDT)) +
  geom_boxplot() +
  labs(title = "Togus Pond Secchi Disk Transparency by Decade", y = 'SDT (m)',x = '')
print(p)
dev.off()


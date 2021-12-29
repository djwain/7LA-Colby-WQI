# Creates box plots by decade using all the data

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Phosphorus.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'China Lake'
MIDAS1 <- 5448
Pall <- dat %>% filter(MIDAS == MIDAS1)

# MIDAS is identifier for LSM data
lake <- 'China Lake'
MIDAS1 <- 5448
Pall <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
PC <- Pall 

# Make a month column
a <- ymd(PC$Date)
PC$Month <- month(a)
PC$Year <- year(a)
PC$Day <- day(a)
PC$Yday <- yday(PC$Date)

P <- PC %>% filter(Yday >= 105 & Yday <= 288)

P1 <- P %>% filter(Station== 1 & Depth >= 21)
Pavgd <- aggregate(`Total P`~Year+Month+Day+Station, mean, data=P1)

Pavgd$Dec <- as.character(floor(Pavgd$Year/10)*10)
Pavgd$Dec <- paste(Pavgd$Dec,'s',sep = "")

png(file="China1_TPBG_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Pavgd %>% filter(Station == 1)
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Total P`)) +
  geom_boxplot() +
  labs(title = "China Lake St. 1 Bottom Grab Total P by Decade", y = 'TP - BG (ppb)',x = '')
print(p)
dev.off()

P2 <- P %>% filter(Station== 2 & Depth >= 14)
Pavgd <- aggregate(`Total P`~Year+Month+Day+Station, mean, data=P2)

Pavgd$Dec <- as.character(floor(Pavgd$Year/10)*10)
Pavgd$Dec <- paste(Pavgd$Dec,'s',sep = "")

png(file="China2_TPBG_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Pavgd %>% filter(Station == 2)
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Total P`)) +
  geom_boxplot() +
  labs(title = "China Lake St. 2 Bottom Grab Total P by Decade", y = 'TP - BG (ppb)',x = '')
print(p)
dev.off()

P3 <- P %>% filter(Station== 3 & Depth >= 14)
Pavgd <- aggregate(`Total P`~Year+Month+Day+Station, mean, data=P3)

Pavgd$Dec <- as.character(floor(Pavgd$Year/10)*10)
Pavgd$Dec <- paste(Pavgd$Dec,'s',sep = "")

png(file="China3_TPBG_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Pavgd %>% filter(Station == 3)
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Total P`)) +
  geom_boxplot() +
  labs(title = "China Lake St. 3 Bottom Grab Total P by Decade", y = 'TP - BG (ppb)',x = '')
print(p)
dev.off()
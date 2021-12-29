# Creates box plots by decade using all the data

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
lake <- 'McGrath Pond'
MIDAS1 <- 5348
Pall <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
PC <- Pall %>% filter(`Sample Type` == 'C')

# Make a month column
a <- ymd(PC$Date)
PC$Month <- month(a)
PC$Year <- year(a)
PC$Day <- day(a)
PC$Yday <- yday(PC$Date)

P <- PC %>% filter(Yday >= 105 & Yday <= 288)
Pavgd <- aggregate(`Total P`~Year+Month+Day+Station, mean, data=P)

Pavgd$Dec <- as.character(floor(Pavgd$Year/10)*10)
Pavgd$Dec <- paste(Pavgd$Dec,'s',sep = "")

png(file="MP1_TP_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Pavgd %>% filter(Station == 1)
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = `Total P`)) +
  geom_boxplot() +
  labs(title = "McGrath Pond St. 1 Epicore Total P by Decade", y = 'TP - EC (ppb)',x = '')
print(p)
dev.off()

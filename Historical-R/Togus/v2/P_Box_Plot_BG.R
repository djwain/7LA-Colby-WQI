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
lake <- 'Togus Pond'
MIDAS1 <- 9931
Pall <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
PB <- Pall %>% filter(Depth >= 13)

# Make a month column
a <- ymd(PB$Date)
PB$Month <- month(a)
PB$Year <- year(a)
PB$Dec <- as.character(floor(PB$Year/10)*10)
PB$Dec <- paste(PB$Dec,'s',sep = "")

# Use MAy-Oct because they have an order of magnitude more measurements than other months
S <- PB %>% filter(Month >= 5 & Month <= 10) %>% filter(Station ==1)
S <- distinct(S)

png(file="TP1_TP_BG_box.png", units="in",width=5, height=5.5, res=400)

# Make box plot by year and by site
p <- ggplot(data = S, aes(x = Dec,y = `Total P`)) +
  geom_boxplot() +
  labs(title = "Togus Pond TP-BG by Decade", y = 'TP (ppb)',x = '')
print(p)
dev.off()




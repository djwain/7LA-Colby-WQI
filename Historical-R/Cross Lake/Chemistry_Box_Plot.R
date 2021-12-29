# Creates box plots by decade using all the data

# Load packages
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_pHColorCond_Alk_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'Cross Lake'
MIDAS1 <- 1674
Chem <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
ChemC <-Chem %>% filter(`Type` == 'C')

# Make a month column
a <- ymd(ChemC$Date)
ChemC$Month <- month(a)
ChemC$Year <- year(a)
ChemC$Dec <- as.character(floor(ChemC$Year/10)*10)
ChemC$Dec <- paste(ChemC$Dec,'s',sep = "")

# Use MAy-Oct because they have an order of magnitude more measurements than other months
S <- ChemC %>% filter(Month >= 5 & Month <= 10) %>% filter(Station ==1)
S <- distinct(S)

png(file="CL1_Col_box.png", units="in",width=5, height=5.5, res=400)

# Make box plot by year and by site
p <- ggplot(data = S, aes(x = Dec,y = `Color (SPU)`)) +
  geom_boxplot() +
  labs(title = "Cross Lake Color by Decade", y = 'Color (SPU)',x = '')
print(p)
dev.off()

png(file="CL1_Cond_box.png", units="in",width=5, height=5.5, res=400)

# Make box plot by year and by site
p <- ggplot(data = S, aes(x = Dec,y = `Conductivity (uS)`)) +
  geom_boxplot() +
  labs(title = "Cross Lake Conductivity by Decade", y = 'Cond (uS)',x = '')
print(p)
dev.off()

png(file="CL1_Alk_box.png", units="in",width=5, height=5.5, res=400)

# Make box plot by year and by site
p <- ggplot(data = S, aes(x = Dec,y = `Alkalinity (mg/L)`)) +
  geom_boxplot() +
  labs(title = "Cross Lake Alkalinity by Decade", y = 'Alk (mg/L)',x = '')
print(p)
dev.off()


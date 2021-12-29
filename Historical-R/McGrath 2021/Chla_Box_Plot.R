# Creates box plots by decade using all the data

# Load packages
library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Chlorophyll_ByDate.xlsx",sep="")
dat <- read_excel(filename, sheet = 2)

# MIDAS is identifier for LSM data
lake <- 'McGrath Pond'
MIDAS1 <- 5348
Chl <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
ChlC <- Chl %>% filter(`Sample Type` == 'C')

# Make a month column
a <- ymd(ChlC$Date)
ChlC$Month <- month(a)
ChlC$Year <- year(a)
ChlC$Day <- day(a)
ChlC$Yday <- yday(ChlC$Date)

C <- ChlC %>% filter(Yday >= 105 & Yday <= 288)
Cavgd <- aggregate(CHLA~Year+Month+Day+Station, mean, data=C)

Cavgd$Dec <- as.character(floor(Cavgd$Year/10)*10)
Cavgd$Dec <- paste(Cavgd$Dec,'s',sep = "")

png(file="MP1_CHLA_box.png", units="in",width=5, height=5.5, res=400)

S1 <- Cavgd %>% filter(Station == 1)
# Make box plot by year and by site
p <- ggplot(data = S1, aes(x = Dec,y = CHLA)) +
  geom_boxplot() +
  labs(title = "McGrath Pond St. 1 Chlorophyll-a by Decade", y = 'ChlA (ppb)',x = '')
print(p)
dev.off()

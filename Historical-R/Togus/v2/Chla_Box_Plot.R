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
lake <- 'Togus Pond'
MIDAS1 <- 9931
Chl <- dat %>% filter(MIDAS == MIDAS1)

# Find epicore data
ChlC <- Chl %>% filter(`Sample Type` == 'C')

# Make a month column
a <- ymd(ChlC$Date)
ChlC$Month <- month(a)
ChlC$Year <- year(a)
ChlC$Dec <- as.character(floor(ChlC$Year/10)*10)
ChlC$Dec <- paste(ChlC$Dec,'s',sep = "")

# Use MAy-Oct because they have an order of magnitude more measurements than other months
S <- ChlC %>% filter(Month >= 5 & Month <= 10) %>% filter(Station ==1)
S <- distinct(S)

png(file="TP1_CHLA_box.png", units="in",width=5, height=5.5, res=400)

# Make box plot by year and by site
p <- ggplot(data = S, aes(x = Dec,y = CHLA)) +
  geom_boxplot() +
  labs(title = "Togus Pond Chlorophyll-a by Decade", y = 'CHLA (ppb)',x = '')
print(p)
dev.off()



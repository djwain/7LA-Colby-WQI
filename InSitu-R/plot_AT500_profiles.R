# This script will plot raw data profiles from the AquaTroll 500
# DJW 31AUG20

library(stringr)
library(ggplot2)

dat <- read.csv(file.choose(new = FALSE), skip =  20,header = FALSE, stringsAsFactors = FALSE)
colnames(dat) <- c('Date','Chl-a Fluor','Turb','Depth','Temp','Act Cond','Spec Cond','Sal','Res','Dens','TDS','RDO Conc',
              'RDO Sat','OPP','junk','Pres','junk3','junk2','Chla.Conc','TSS','a')

# Trim data so only downcast is used
maxdepi <-which.max(dat$Depth)
dat <- dat[1:maxdepi,]


dat2 <- read.csv(file.choose(new = FALSE), skip =  20,header = FALSE, stringsAsFactors = FALSE)
colnames(dat2) <- c('Date','Turb','TSS','Act Cond','Spec Cond','Sal','TDS','Res','Dens','RDO Conc',
                   'RDO Sat','OPP','Temp','Pres','Depth','Chl-a Fluor','Chla.Conc','a','b','c','d','e','f')

# Trim data so only downcast is used
maxdepi <-which.max(dat2$Depth)
dat2 <- dat2[1:maxdepi,]

# ggplot() +
#   geom_point(data = dat,aes(x = Chla.Conc, y = Depth, color = 'North')) +
#   geom_point(data = dat2,aes(x = Chla.Conc, y = Depth, color = 'Little North')) +
#   labs(x = "Chl-a Concentration (ug/L)", y = 'Depth (m)') +
#   scale_y_continuous(trans = "reverse",limits = c(5,0)) +
#   scale_x_continuous(limits = c(0,25)) +
#   scale_colour_manual("", values = c("North"="red", "Little North"="blue"))
  
ggplot()+
  geom_point(data = dat, aes(x = (Temp*9/5+32), y = Depth*3.28)) +
  labs(x = "Temperature (F)", y = 'Depth (ft)') +
  scale_y_continuous(trans = "reverse")

ggplot()+
  geom_point(data = dat, aes(x = `RDO Conc`, y = Depth*3.28)) +
  labs(x = "Dissolved Oxygen (ppm)", y = 'Depth (ft)') +
  scale_y_continuous(trans = "reverse")

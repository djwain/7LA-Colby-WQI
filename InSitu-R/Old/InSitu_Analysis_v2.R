library(stringr)
library(rLakeAnalyzer)
library(dplyr)

# Load data
dat <- file.choose()
suppressWarnings({
  junk <- read.csv(dat, header = FALSE, stringsAsFactors = FALSE)
  if(str_detect(junk,"GPS") == TRUE) {
    skipno = 10
  }else {
    skipno = 9
  }
})
Insit.T <- read.csv(dat, skip = skipno, header = TRUE, stringsAsFactors = FALSE)

# Trim data so only downcast is used
maxdepi <-which.max(Insit.T$Depth..m.)
Dep2 <- Insit.T$Depth..m.[1:maxdepi]
Temp2 <- Insit.T$Temp..C.[1:maxdepi]
RDO2 <- Insit.T$RDO..mg.L.[1:maxdepi]

#Find Bottom Temp
Bottom.Temp <- min(Temp2)

#Find 1-m Temp
surftemp <-which(Dep2 >0.9 & Dep2 <1.1)
surfDATA <- Insit.T[surftemp,]
Surface.Temp <-mean(surfDATA$Temp..C.)

#Find anoxic Depth
anoxia <- which(RDO2 <2)
AnoxDATA <- Insit.T[anoxia,]
Anoxic.Depth.m. <- min(AnoxDATA$Depth..m.)

#Find Thermocline Depth
ThermDAT <- Insit.T %>% select(Depth..m., Temp..C.) %>% 
  group_by(Depth..m.) %>% 
  summarise(
    Temp..C.= mean(Temp..C.)
  )
Thermocline.Depth.m. <-thermo.depth(ThermDAT$Temp..C., ThermDAT$Depth..m., Smin=1, seasonal = FALSE)
Thermocline.Depth.m.


#Find Date
Date <-Insit.T$Created[1]
Date
#Make Table of Summary Data

Insit.Sum <- data.frame(Thermocline.Depth.m., Surface.Temp, Bottom.Temp, Anoxic.Depth.m., Date)


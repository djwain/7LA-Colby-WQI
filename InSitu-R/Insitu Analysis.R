setwd('/Users/Charlie/Desktop/MLRC Work')
#if symbols (greek letters) are in header of csv, you must delete them before downloading 
Insit.T <-read.csv('NPDEP1_20180621.test.csv')
View(Insit.T)
#Find Bottom Temp
Bottom.Temp <- min(Insit.T$Temp..C.)
#Find 1-m Temp
surftemp <-which(Insit.T$Depth..m. >0.9 & Insit.T$Depth..m. <1.1)
surfDATA <- Insit.T[surftemp,]
Surface.Temp <-mean(surfDATA$Temp..C.)
#Find anoxic Depth
anoxia <-which(Insit.T$RDO..mg.L. <2)
AnoxDATA <-Insit.T[anoxia,]
Anoxic.Depth.m. <-min(AnoxDATA$Depth..m.)

#Find Thermocline Depth
install.packages("rLakeAnalyzer")
library("rLakeAnalyzer")
?rLakeAnalyzer
library(dplyr)

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


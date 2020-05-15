# This script mines the spreadsheets from DEP and LSM to finds the MIDAS number of lakes that:
# 1) are greater than 10 m
# 2) have hypsographs
# 3) have at least 10 T and DO profiles
# 4) have at least 10 P measurements
# DJW 4/8/20

library(readxl)
library(dplyr)
library(lubridate)

# First step is to find the deep lakes
Morph <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Morphology/MaineLakes_Geography_Morphometry.xls", sheet = 2)
min_depth <- 10*3.28 # depths are in ft
MIDAS_all <- Morph$`Lake Code (numeric)`

Morph1 <- Morph %>% 
  filter(`Depth_Max (feet)` > min_depth)
deep_lakes <- Morph1$`Lake Code (numeric)`

# Next find the lakes with hypsograph data
Hypso <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Morphology/LakeContourFiles.xlsx")
hypso_lakes <- unique(Hypso$MIDAS)

# Find deep lakes with hypsograph data
indx <- match(hypso_lakes, deep_lakes, nomatch = NA_integer_, incomparables = NULL)
deep_hypso_lakes <- deep_lakes[indx]
deep_hypso_lakes <- deep_hypso_lakes[!is.na(deep_hypso_lakes)]

# Next the number of T/DO profiles available for each lake
TDO <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/MaineLakes_Temp_DO.xlsx", sheet = 2)
by_lake <- group_by(TDO, MIDAS)
summarise(by_lake, nprofs = length(unique(Date)))

TO$month <- month(TO$Date)
TO$MIDAS <- as.numeric(TO$MIDAS)
TO1 <- TO %>% 
  filter(month > 0 & month < 6)
spring_lakes <- unique(TO1$MIDAS)

# Find deep lakes with ice data and spring data
indx2 <- match(deep_ice_lakes, spring_lakes, nomatch = NA_integer_, incomparables = NULL)
deep_ice_spring_lakes <- spring_lakes[indx2]
deep_ice_spring_lakes <- deep_ice_spring_lakes[!is.na(deep_ice_spring_lakes)]

# Subset all data to export for further analysis
TOdis <- subset(TO, MIDAS %in% deep_ice_spring_lakes)
Icedis <- subset(Ice, MIDAS %in% deep_ice_spring_lakes)
Morphdis <- subset(Morph, `Lake Code (numeric)` %in% deep_ice_spring_lakes)

write.csv(TOdis, file = "TO_DIS.csv") 
write.csv(Icedis, file = "Ice_DIS.csv") 
write.csv(Morphdis, file = "Morph_DIS.csv") 



# This script mines the spreadsheets from DEP and LSM to finds the MIDAS number of lakes that:
# 1) are greater than 10 m
# 2) the years for which there is ice off data
# 3) the years for which there are spring profiles
# DJW 2/26/20

library(readxl)
library(dplyr)
library(lubridate)

# First step is to find the deep lakes
Morph <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Morphology/MaineLakes_Geography_Morphometry.xls", sheet = 2)
min_depth <- 10*3.28 # depths are in ft

Morph1 <- Morph %>% 
  filter(`Depth_Max (feet)` > min_depth)
deep_lakes <- Morph1$`Lake Code (numeric)`

# Next find the lakes with ice off data
Ice <- read.csv("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/Ice Data Spreadsheet combined v2.csv", header = TRUE, stringsAsFactors = FALSE)
ice_lakes <- unique(Ice$MIDAS)

# Find deep lakes with ice data
indx <- match(ice_lakes, deep_lakes, nomatch = NA_integer_, incomparables = NULL)
deep_ice_lakes <- deep_lakes[indx]
deep_ice_lakes <- deep_ice_lakes[!is.na(deep_ice_lakes)]

# Next find lakes with spring profiles (before June)
TO <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/MaineLakes_Temp_DO.xlsx", sheet = 2)
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

write.csv(TOdis, file = "TO_DIS.csv",row.names=FALSE) 
write.csv(Icedis, file = "Ice_DIS.csv",row.names=FALSE) 
write.csv(Morphdis, file = "Morph_DIS.csv",row.names=FALSE) 



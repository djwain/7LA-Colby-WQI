# This script mines the spreadsheets from DEP and LSM to finds the MIDAS number of lakes that:
# 1) are greater than 10 m
# 2) have hypsographs
# 3) have at least 10 T and DO profiles
# 4) have at least 10 P profiles
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
TDOprofs <-summarise(by_lake, nprofs = length(unique(Date)))
TDO_10 <- filter(TDOprofs, nprofs >= 10)
TDO_lakes <- as.numeric(TDO_10$MIDAS)

# Find deep lakes with hypsographs and 10 T and DO profiles
indx2 <- match(deep_hypso_lakes, TDO_lakes, nomatch = NA_integer_, incomparables = NULL)
deep_hypso_TDO_lakes <- TDO_lakes[indx2]
deep_hypso_TDO_lakes <- deep_hypso_TDO_lakes[!is.na(deep_hypso_TDO_lakes)]

# Now add those with P profiles
P <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/MaineLakes_Phosphorus.xlsx", sheet = 2)
PPG <- P %>% filter(Qualifier == "PG")
Pby_lake <- group_by(PPG, MIDAS)
Pprofs <-summarise(Pby_lake, nprofs = length(unique(Date)))
P_10 <- filter(Pprofs, nprofs >= 10)
P_lakes <- as.numeric(P_10$MIDAS)

# Find deep lakes with hypsographs and 10 T and DO profiles and 10 P profiles
indx3 <- match(deep_hypso_TDO_lakes, P_lakes, nomatch = NA_integer_, incomparables = NULL)
deep_hypso_TDO_P_lakes <- P_lakes[indx3]
deep_hypso_TDO_P_lakes <- deep_hypso_TDO_P_lakes[!is.na(deep_hypso_TDO_P_lakes)]

# Subset all data to export for further analysis
MorphAbA <- subset(Morph, `Lake Code (numeric)` %in% deep_hypso_TDO_P_lakes)
HypsoAbA <- subset(Hypso, MIDAS %in% deep_hypso_TDO_P_lakes)
TDOAbA <- subset(TDO, as.numeric(MIDAS) %in% deep_hypso_TDO_P_lakes)
PAbA <- subset(PPG, as.numeric(MIDAS) %in% deep_hypso_TDO_P_lakes)


write.csv(MorphAbA, file = "MorphAbA.csv",row.names=FALSE) 
write.csv(HypsoAbA, file = "HypsoAbA.csv",row.names=FALSE) 
write.csv(TDOAbA, file = "TDOAbA.csv",row.names=FALSE) 
write.csv(PAbA, file = "PAbA.csv",row.names=FALSE) 


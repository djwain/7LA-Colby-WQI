# This script uses the data mined from the LSM/DEP database for deep (>10m) lakes with both
# ice off data and profiles before June. Here we will reduce the data even further, removing
# years where we don't have both ice and spring data.

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)     

TO <- read.csv('TO_DIS.csv',header = TRUE, stringsAsFactors = FALSE)
Ice <- read.csv('Ice_DIS.csv',header = TRUE, stringsAsFactors = FALSE)
Morph <- read.csv('Morph_DIS.csv',header = TRUE, stringsAsFactors = FALSE)

TO$Year <- year(TO$Date)

# Find years with profiles before June
TO1 <- TO %>% group_by(MIDAS,Year) %>%
  summarise(prof1 = min(Date)) %>%
  filter(month(prof1) > 0 & month(prof1) < 6)

# Find DT of all profiles
TO2 <- TO %>% filter(STATION == 1) %>% group_by(MIDAS,Date,month) %>%
  summarise(DT = max(TEMPERATURE)-min(TEMPERATURE), npts = length(DEPTH)) %>%
  filter(npts > 1)

TO3 <- merge(TO1,TO2, by.x=c("MIDAS", "prof1"), by.y=c("MIDAS", "Date")) %>%
arrange(MIDAS)

# Change format of year in ice data
a <- mdy(Ice$Ice.Out)
Ice$Year <- year(a)

# Merge dataframes
TO4 <- merge(TO3,Ice)
TO5 <- select(TO4,MIDAS,Year,prof1,Ice.Out,DT) %>%
  arrange(MIDAS)

# Filter by first profiles that have DT < 2
TO6 <- TO5 %>%
  filter(DT < 2)

# Alternative: Merge T02 with Ice so that we can see when the first instance of DT > 2 is
b <- ymd(TO2$Date)
TO2$Year <- year(b)
TO7 <-merge(TO2, Ice, by=c("Year","MIDAS"))
TO8 <- select(TO7,MIDAS,Year,month,Date,Ice.Out,DT,npts)%>%
  arrange(MIDAS,Year,month)

TO9 <- distinct(TO8)

T10 <- TO9 %>% group_by(MIDAS,Year) %>%
  summarise(prof1 = min(Date), DT1 = DT[1], prof2 = Date[min(which(DT > 2))], DT2 = DT[min(which(DT > 2))])

yr0 = as.numeric(T10$Year) - 1
T10$yd1 <- as.numeric(as.Date(T10$prof1)-as.Date(0, origin=paste(yr0,"12-31",sep = '-')))
T10$yd2 <- as.numeric(as.Date(T10$prof2)-as.Date(0, origin=paste(yr0,"12-31",sep = '-')))

T11 <- T10 %>% filter(yd2 - yd1 > 0)
T11$stratyd <- approx(c(T11$DT1,T11$DT2),c(T11$yd1,T11$yd2),2)

# Need to use approx() but need to do it on a row by row basis. Trying to figure this out.




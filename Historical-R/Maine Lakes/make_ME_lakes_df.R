# Create data frame with the Maine lakes historical data
# DJW 4/28/21

# Create a data frame that contains merges all Maine Lakes data calculates a few metrics
# zS_m = Secchi disk transparency (m)
# Ts_C = Surface Temperature (C) (average of z <= 1 m)
# zT_m = Thermocline Depth (m) - first depth where dT/dz >= 1 C/m
# zhypox_m = Hypoxic Depth (m) - first depth where DO < 5
# zanox_m = Anoxic Depth (m) - first depth where DO < 2
# Ps_ppb = Surface phosphorus (ppb) from epicores
# Pb_ppb = Bottom Phosphorus (ppb) from bottom grabs
# Chla_ppb = Chlorophyll-a concentration (ppb) from epicore

setwd("~/Documents/GitHub/7LA-Colby-WQI/Historical-R/Maine Lakes")

# Load packages
library(readxl)
library(tidyverse)
library(lubridate)

# Load secchi data from LSM
filename0 <- paste("MaineLakes_Secchi_ByDate.xlsx",sep="")
S0 <- read_excel(filename0, sheet = 2)

S1 <- S0 %>% filter(STATION == 1)
S2 <- S1 %>% rename(midas = `Lake Code (MIDAS)`, zS_m = `SECCHI DEPTH`)
a <- ymd(S2$DATE)
S2$year <- year(a)
S2$month <- month(a)
S2$yday <- yday(a)

WQ0 <- S2 %>% select(midas, year, month, yday, zS_m )

# Load TO data from LSM
filename1 <- paste("MaineLakes_Temp_DO.xlsx",sep="")
T0 <- read_excel(filename1, sheet = 2)

T1 <- T0 %>% filter(STATION == 1)

# Filter by z <= 1 m
T2 <- T1 %>% filter(DEPTH <= 1)

# Average any points from the same day
T3 <- T2 %>% group_by(MIDAS, Date) %>% summarise(Ts_C = round(mean(TEMPERATURE),1))
T4 <- T3 %>% rename(midas = MIDAS)
a <- ymd(T4$Date)
T4$year <- year(a)
T4$month <- month(a)
T4$yday <- yday(a)

T5 <- T4 %>% select(midas, year, month, yday, Ts_C )

WQ1 <- merge(WQ0, T5, all = TRUE)

# Find thermocline depth based on DEP standard dT/dz >= 1 C/m
T6 <- T1 %>% group_by(MIDAS, Date) %>% summarize(z = as.numeric(DEPTH), dTdz = c(diff(TEMPERATURE)/diff(as.numeric(DEPTH)),NA))
T7 <- T6 %>% group_by(MIDAS, Date) %>% summarize(zT_m = z[min(which(dTdz <= -1))])

T8 <- T7 %>% rename(midas = MIDAS)
a <- ymd(T8$Date)
T8$year <- year(a)
T8$month <- month(a)
T8$yday <- yday(a)

T9 <- T8 %>% select(midas, year, month, yday, zT_m )

WQ2 <- merge(WQ1, T9, all = TRUE)

# Find hypoxic depth (DO < 5 ppm) and anoxic depth (DO < 2 ppm)
T1$DEPTH <- as.numeric(T1$DEPTH)
T10 <- T1 %>% group_by(MIDAS, Date) %>% 
  summarize(zhypox_m = DEPTH[min(which(OXYGEN < 5))], zanox_m = DEPTH[min(which(OXYGEN < 2))])

T11 <- T10 %>% rename(midas = MIDAS)
a <- ymd(T11$Date)
T11$year <- year(a)
T11$month <- month(a)
T11$yday <- yday(a)

T12 <- T11 %>% select(midas, year, month, yday, zhypox_m, zanox_m )

WQ3 <- merge(WQ2, T12, all = TRUE)

# Load TP data from LSM
filename2 <- paste("MaineLakes_Phosphorus.xlsx",sep="")
P0 <- read_excel(filename2, sheet = 2)

P1 <- P0 %>% filter(Station == 1)

P2 <- P1 %>% filter(Qualifier == "EC")
P3 <- P2 %>% group_by(MIDAS, Date) %>% summarize(Ps_ppb = `Total P`) %>% ungroup

P4 <- P3 %>% rename(midas = MIDAS)
a <- ymd(P4$Date)
P4$year <- year(a)
P4$month <- month(a)
P4$yday <- yday(a)

P5 <- P4 %>% select(midas, year, month, yday, Ps_ppb )

WQ4 <- merge(WQ3, P5, all = TRUE)

# Now bottom grabs
P6 <- P1 %>% filter(Qualifier == "BG")
P7 <- P6 %>% group_by(MIDAS, Date) %>% summarize(Pb_ppb = `Total P`) %>% ungroup

P8 <- P7 %>% rename(midas = MIDAS)
a <- ymd(P8$Date)
P8$year <- year(a)
P8$month <- month(a)
P8$yday <- yday(a)

P9 <- P8 %>% select(midas, year, month, yday, Pb_ppb )

WQ5 <- merge(WQ4, P9, all = TRUE)

# Load Chl-a data from LSM and find core data
filename3 <- paste("MaineLakes_Chlorophyll_ByDate.xlsx",sep="")
C0 <- read_excel(filename3, sheet = 2)

C1 <- C0 %>% filter(Station == 1)

C2 <- C1 %>% filter(`Sample Type` == "C")
C3 <- C2 %>% group_by(MIDAS, Date) %>% summarize(Chla_ppb = CHLA) %>% ungroup

C4 <- C3 %>% rename(midas = MIDAS)
a <- ymd(C4$Date)
C4$year <- year(a)
C4$month <- month(a)
C4$yday <- yday(a)

C5 <- C4 %>% select(midas, year, month, yday, Chla_ppb )

WQ6 <- merge(WQ5, C5, all = TRUE)


save(WQ6,file = "ME_daily_metrics.Rda")



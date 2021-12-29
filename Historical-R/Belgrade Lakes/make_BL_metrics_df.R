# Create data frame with the Belgrade Lake historical data
# DJW 4/28/21

# Create a data frame that contains monthlu average data from April - Oct
# zS_m = Secchi disk transparency (m)
# Ts_C = Surface Temperature (C)
# Tb_C = Bottom Temperature (C)
# DOb_ppm = Bottom DO (ppm)
# zT_m = Thermocline Depth (m) - first depth where dT/dz >= 1 C/m
# zhypox_m = Hypoxic Depth (m) - first depth where DO < 5
# zanox_m = Anoxic Depth (m) - first depth where DO < 2
# Ps_ppb = Surface phosphorus (ppb) from epicores
# Pb_ppb = Bottom Phosphorus (ppb) from bottom grabs


# Load packages
library(readxl)
library(tidyverse)
library(lubridate)

# Load secchi data from LSM
filepath <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/"
filename <- paste(filepath,"MaineLakes_Secchi_ByDate.xlsx",sep="")
S0 <- read_excel(filename, sheet = 2)

# Belgrade Lake MIDAS numbers 
BL_MIDAS <- c(5349, 5344, 5348, 5352, 5274, 5272, 5280)
S1 <- S0 %>% filter(`Lake Code (MIDAS)` == BL_MIDAS)

# Use only station 1, except for Long Pond where we treat 1 and 2 separately
SLP2 <- S1 %>% filter(`Lake Code (MIDAS)` == 5272 & STATION == 2)
SLP2$LAKE <- 'Long_Lower'

S2 <- S1 %>% filter(STATION == 1)
S3 <- rbind(S2, SLP2)

S3$LAKE <- replace(S3$LAKE, S3$LAKE == 'East Pond', 'East')
S3$LAKE <- replace(S3$LAKE, S3$LAKE == 'North Lake (Little Pond)', 'North')
S3$LAKE <- replace(S3$LAKE, S3$LAKE == 'McGrath Pond', 'McGrath')
S3$LAKE <- replace(S3$LAKE, S3$LAKE == 'Salmon Pond (Ellis Pond)', 'Salmon')
S3$LAKE <- replace(S3$LAKE, S3$LAKE == 'Great Pond', 'Great')
S3$LAKE <- replace(S3$LAKE, S3$LAKE == 'Long Pond', 'Long_Upper')
S3$LAKE <- replace(S3$LAKE, S3$LAKE == 'Messalonskee Lake (Snow Pond)', 'Messalonskee')

S4 <- S3 %>% rename(lake = LAKE, zS_m = `SECCHI DEPTH`)
a <- ymd(S4$DATE)
S4$year <- year(a)
S4$month <- month(a)
S4$yday <- yday(a)

WQ0 <- S4 %>% select(lake, year, month, yday, zS_m )

# Load TO data from LSM
# Find profiles from these MIDAS numbers
T0 <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/MaineLakes_Temp_DO.xlsx", sheet = 2)
T1 <- T0 %>% filter(MIDAS %in% BL_MIDAS)

# Use only station 1, except for Long Pond where we treat 1 and 2 separately
TLP2 <- T1 %>% filter(MIDAS == 5272 & STATION == 2)
TLP2$LAKE <- 'Long_Lower'

T2 <- T1 %>% filter(STATION == 1)
T3 <- rbind(T2, TLP2)

T3$LAKE <- replace(T3$LAKE, T3$LAKE == 'East Pond', 'East')
T3$LAKE <- replace(T3$LAKE, T3$LAKE == 'North Lake (Little Pond)', 'North')
T3$LAKE <- replace(T3$LAKE, T3$LAKE == 'McGrath Pond', 'McGrath')
T3$LAKE <- replace(T3$LAKE, T3$LAKE == 'Salmon Pond (Ellis Pond)', 'Salmon')
T3$LAKE <- replace(T3$LAKE, T3$LAKE == 'Great Pond', 'Great')
T3$LAKE <- replace(T3$LAKE, T3$LAKE == 'Long Pond', 'Long_Upper')
T3$LAKE <- replace(T3$LAKE, T3$LAKE == 'Messalonskee Lake (Snow Pond)', 'Messalonskee')

# Filter by z <= 1 m
T4 <- T3 %>% filter(DEPTH <= 1)

# Average any points from the same day
T5 <- T4 %>% group_by(LAKE, Date) %>% summarise(Ts_C = round(mean(TEMPERATURE),1))
T6 <- T5 %>% rename(lake = LAKE)
a <- ymd(T6$Date)
T6$year <- year(a)
T6$month <- month(a)
T6$yday <- yday(a)

T7 <- T6 %>% select(lake, year, month, yday, Ts_C )

WQ1 <- merge(WQ0, T7, all = TRUE)

# Find the bottom temperatures
load(BL_morph.Rda)
morph0 <- morph %>% rename(LAKE = Lake)
T8 <- merge(T3,morph0)
T8$HAB <- T8$Max_Depth_m - as.numeric(T8$DEPTH)

# Filter by z <= 1 m
T9 <- T8 %>% filter(HAB <= 1.5)

# Average any points from the same day
T10<- T9 %>% group_by(LAKE, Date) %>% summarise(Tb_C = round(mean(TEMPERATURE),1),DOb_ppm = round(mean(as.numeric(OXYGEN)),1))
T11 <- T10 %>% rename(lake = LAKE)
a <- ymd(T11$Date)
T11$year <- year(a)
T11$month <- month(a)
T11$yday <- yday(a)

T12 <- T11 %>% select(lake, year, month, yday, Tb_C, DOb_ppm )

WQ2 <- merge(WQ1, T12, all = TRUE)

# Find thermocline depth based on DEP standard dT/dz >= 1 C/m
# Start with T8
T13 <- T8 %>% group_by(LAKE, Date) %>% summarize(z = as.numeric(DEPTH), dTdz = c(diff(TEMPERATURE)/diff(as.numeric(DEPTH)),NA))
T14 <- T13 %>% group_by(LAKE, Date) %>% summarize(zT_m = z[min(which(dTdz <= -1))])

T15 <- T14 %>% rename(lake = LAKE)
a <- ymd(T15$Date)
T15$year <- year(a)
T15$month <- month(a)
T15$yday <- yday(a)

T16 <- T15 %>% select(lake, year, month, yday, zT_m )

WQ3 <- merge(WQ2, T16, all = TRUE)

# Find hypoxic depth (DO < 5 ppm) and anoxic depth (DO < 2 ppm)
T8$DEPTH <- as.numeric(T8$DEPTH)
T17 <- T8 %>% group_by(LAKE, Date) %>% 
  summarize(zhypox_m = DEPTH[min(which(OXYGEN < 5))], zanox_m = DEPTH[min(which(OXYGEN < 2))])

T18 <- T17 %>% rename(lake = LAKE)
a <- ymd(T18$Date)
T18$year <- year(a)
T18$month <- month(a)
T18$yday <- yday(a)

T19 <- T18 %>% select(lake, year, month, yday, zhypox_m, zanox_m )

WQ4 <- merge(WQ3, T19, all = TRUE)

# Load TP data from LSM
# Find profiles from these MIDAS numbers
P0 <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/MaineLakes_Phosphorus.xlsx", sheet = 2)
P1 <- P0 %>% filter(MIDAS %in% BL_MIDAS)

# Use only station 1, except for Long Pond where we treat 1 and 2 separately
PLP2 <- P1 %>% filter(MIDAS == 5272 & Station == 2)
PLP2$Lake <- 'Long_Lower'

P2 <- P1 %>% filter(Station == 1)
P3 <- rbind(P2, PLP2)

P3$Lake <- replace(P3$Lake, P3$Lake == 'East Pond', 'East')
P3$Lake <- replace(P3$Lake, P3$Lake == 'North Lake (Little Pond)', 'North')
P3$Lake <- replace(P3$Lake, P3$Lake == 'McGrath Pond', 'McGrath')
P3$Lake <- replace(P3$Lake, P3$Lake == 'Salmon Pond (Ellis Pond)', 'Salmon')
P3$Lake <- replace(P3$Lake, P3$Lake == 'Great Pond', 'Great')
P3$Lake <- replace(P3$Lake, P3$Lake == 'Long Pond', 'Long_Upper')
P3$Lake <- replace(P3$Lake, P3$Lake == 'Messalonskee Lake (Snow Pond)', 'Messalonskee')

P4 <- P3 %>% filter(Qualifier == "EC")
P5 <- P4 %>% group_by(Lake, Date) %>% summarize(Ps_ppb = `Total P`) %>% ungroup

P6 <- P5 %>% rename(lake = Lake)
a <- ymd(P6$Date)
P6$year <- year(a)
P6$month <- month(a)
P6$yday <- yday(a)

P7 <- P6 %>% select(lake, year, month, yday, Ps_ppb )

WQ5 <- merge(WQ4, P7, all = TRUE)

# Now bottom grabs
P8 <- P3 %>% filter(Qualifier == "BG")
P9 <- P8 %>% group_by(Lake, Date) %>% summarize(Pb_ppb = `Total P`) %>% ungroup

P10 <- P9 %>% rename(lake = Lake)
a <- ymd(P10$Date)
P10$year <- year(a)
P10$month <- month(a)
P10$yday <- yday(a)

P11 <- P10 %>% select(lake, year, month, yday, Pb_ppb )

WQ6 <- merge(WQ5, P11, all = TRUE)

# Find stratification onset date?
strat0 <- WQ6 %>% group_by(lake,year) %>% 
  summarize(strat_on = yday[min(which(!is.na(zT_m)))],
            strat_off = yday[max(which(!is.na(zT_m)))],
            strat_days = strat_off-strat_on,
            hypox_on = yday[min(which(!is.na(zhypox_m)))],
            hypox_off = yday[max(which(!is.na(zhypox_m)))],
            hypox_days = hypox_off-hypox_on,
            anox_on = yday[min(which(!is.na(zanox_m)))],
            anox_off = yday[max(which(!is.na(zanox_m)))],
            anox_days = anox_off-anox_on,
            med_zS = median(zS_m, na.rm = TRUE))

ice0 <- ice %>% select(Lake, Year, YearDay)
ice1 <- ice0 %>% rename(lake = Lake, year = Year, ice_off = YearDay)

YearMetrics <- merge(ice1,strat0, all = TRUE)
save(YearMetrics,file = "BL_yearly_metrics.Rda")



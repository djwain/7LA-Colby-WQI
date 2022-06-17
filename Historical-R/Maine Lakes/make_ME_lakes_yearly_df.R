# Create data frame with yearly median values of daily data and metrics and computes yearly metrics
# DJW 4/28/21

# zsmed = median yearly Secchi disk transparency (m)
# Ts = median yearly surface temperature (C) (average of z <= 1 m)
# zT = median yearly thermocline depth (m) - first depth where dT/dz >= 1 C/m
# zhypox = median yearly hypoxic depth (m) - first depth where DO < 5
# zanox = median yearly anoxic depth (m) - first depth where DO < 2
# Ps = median yearly surface phosphorus (ppb) from epicores
# Pb = median yearly bottom phosphorus (ppb) from bottom grabs
# Chla = median yearly chlorophyll-a concentration (ppb) from epicore
# strat_on = first day where a thermocline exists
# strat_off = last day where a thermocline exists
# strat_days = strat_off - strat_on (does not acknowledge polymixis well)
# hypox_on = first day where hypoxia exists
# hypox_off = last day where hypoxia exists
# hypox_days = hypox_off - hypox_on (does not acknowledge polymixis well)
# anox_on = first day where anoxia exists
# anox_off = last day where anoxia exists
# anox_days = anox_off - anox_on (does not acknowledge polymixis well)

setwd("~/Documents/GitHub/7LA-Colby-WQI/Historical-R/Maine Lakes")

# Load packages
library(readxl)
library(tidyverse)
library(lubridate)

load("ME_daily_metrics.Rda")

WQ7 <- WQ6 %>%
  group_by(midas, year) %>%
  summarize(zsmed = median(zS_m, na.rm = TRUE), 
            Ts = median(Ts_C, na.rm = TRUE),
            zT = median(zT_m, na.rm = TRUE),
            zhypox = median(zhypox_m, na.rm = TRUE),
            zanox = median(zanox_m, na.rm = TRUE),
            Ps = median(Ps_ppb, na.rm = TRUE),
            Pb = median(Pb_ppb, na.rm = TRUE),
            Chla = median(Chla_ppb, na.rm = TRUE),
            strat_on = yday[min(which(!is.na(zT_m)))],
            strat_off = yday[max(which(!is.na(zT_m)))],
            strat_days = strat_off-strat_on,
            hypox_on = yday[min(which(!is.na(zhypox_m)))],
            hypox_off = yday[max(which(!is.na(zhypox_m)))],
            hypox_days = hypox_off-hypox_on,
            anox_on = yday[min(which(!is.na(zanox_m)))],
            anox_off = yday[max(which(!is.na(zanox_m)))],
            anox_days = anox_off-anox_on
  )

# Load ice off data. Have o do some weird things because of the way R is reading this file
filename <- paste("Ice Data Spreadsheet combined v2.xlsx",sep="")
I0 <- read_excel(filename)

# midas numbers are being read in as numbers, not text, e.g. it is 0007 in Excel but 7 here
I1 <- I0 %>% rename(midas = MIDAS)
junk <- I1$midas+10000
junk1 <- as.character(junk)
junk2 <- str_sub(junk1,2L,-1L)
I1$midas <- junk2

ai <- ymd(I1$`Ice In (final)`)
ice_in <- yday(ai)

I1$`Ice Out` <- as.Date(as.numeric(I1$`Ice Out`), origin = "1899-12-30")
ao <- ymd(I1$`Ice Out`)
I1$ice_out <- yday(ao)
I1$ice_days <- 365 - (ice_in - I1$ice_out)
I1$year <- year(ao)

I2 <- I1 %>% select(midas, year, ice_out, ice_days)

WQ8 <- merge(WQ7, I2, all = TRUE)

save(WQ8,file = "ME_Lakes_yearly_metrics.Rda")


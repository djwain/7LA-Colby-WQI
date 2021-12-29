# Makes a meta data frame of all data
# DJW 6/29/21

library(tidyverse)
library(GGally)

load("BL_climate.Rda")
load("BL_morph.Rda")
load("BL_yearly_metrics.Rda")
load("BL_daily_metrics.Rda")

climate0 <- climate %>% rename(month = Month, year = Year)
dat0 <- merge(WQ6,climate0)

morph0 <- morph %>% rename(lake = Lake)
dat1 <- merge(dat0,morph0)

dat2 <- merge(dat1,YearMetrics[,-13])
bl_all <- dat2

save(bl_all,file = "BL_all_data_merged.Rda")
ggcorr(data = bl_all[,5:39], nbreaks = 8)

zscorr <- bl_all %>% select(zS_m,Tb_C,DOb_ppm,zhypox_m,zanox_m,Ps_ppb, Mean_Depth_m, Max_Depth_m,Osgood, Res_Time_yrs, strat_off, anox_off)
ggcorr(data = zscorr, nbreaks = 8, label = TRUE,label_round = 2)

# Try log transformed variables where appropriate
bl_all$Area_km2 <- log(bl_all$Area_km2)
bl_all$Max_Depth_m <- log(bl_all$Max_Depth_m)
bl_all$Volume_m3 <- log(bl_all$Volume_m3)
bl_all$Res_Time_yrs <- log(bl_all$Res_Time_yrs)
ggcorr(data = bl_all[,5:39], nbreaks = 4)

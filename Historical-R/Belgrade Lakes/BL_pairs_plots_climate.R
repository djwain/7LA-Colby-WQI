# Plot weather correlations
# DJW 5/25/21

library(psych)
library(viridis)
library(GGally)
library(tidyverse)

load("BL_climate.Rda")

ggpairs(climate[,3:12], aes(color = as.character(climate$Month)))



load("BL_daily_metrics.Rda")

ggpairs(data = WQ6, columns = 5:13)

WQ7 <-WQ6 %>% filter(lake == 'Messalonskee')
ggpairs(data = WQ7, columns = 5:13)


load("BL_yearly_metrics.Rda")

ggpairs(data = YearMetrics, columns = 3:13)

YearMetrics1 <-YearMetrics %>% filter(lake == 'Salmon'|lake == 'Great'|lake == 'Long_Upper'|lake == 'Long_Lower'|lake == 'Messalonskee')
ggpairs(data = YearMetrics, columns = 3:13)



load("BL_morph.Rda")

ggpairs(morph[,2:7])



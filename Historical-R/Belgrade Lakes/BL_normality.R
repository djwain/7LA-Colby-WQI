# Checks the normality of the BL predictor variables
# DJW 6/28/21

library(tidyverse)

load("BL_climate.Rda")

temp <- climate$PZI
shapiro.test(temp)
shapiro.test(log(temp))
qqnorm(temp)

load("BL_morph.Rda")

temp <- morph$Res_Time_yrs
shapiro.test(temp)
shapiro.test(log(temp))
qqnorm(temp)
qqnorm(log(temp))

load("BL_yearly_metrics.Rda")

temp <- YearMetrics$anox_days
shapiro.test(temp)
shapiro.test(log(temp))
qqnorm(temp)
qqnorm(log(temp))

load("BL_daily_metrics.Rda")

temp <- WQ6$Pb_ppb
shapiro.test(temp)
shapiro.test(log(temp))
qqnorm(temp)
qqnorm(log(temp))

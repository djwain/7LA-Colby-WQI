# Let's grow a tree!
# DJW 7/3/21

library(rpart)
library(tidyverse)

load("BL_all_data_merged.Rda")

fit <- rpart(formula = zS_m ~ Ps_ppb + DOb_ppm + Tb_C + Res_Time_yrs + Osgood + anox_off, data = bl_all, method = "anova")

plot(fit)
text(fit)

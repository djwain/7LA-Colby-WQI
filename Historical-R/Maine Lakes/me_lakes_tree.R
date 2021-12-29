# ME lakes tree just on daily data

library(rpart)
library(tidyverse)

load("ME_daily_metrics.Rda")

fit <- rpart(formula = zS_m ~ Ts_C + zT_m + zhypox_m + zanox_m + Ps_ppb +Pb_ppb, data = WQ5, method = "anova")

plot(fit)
text(fit, use.n=TRUE)

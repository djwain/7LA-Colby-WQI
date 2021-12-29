# This script uses the daily average temperature to compute the thermocline depth
# and determine when there are mixing events. This is then compared to increases
# chlorophyll

library(rLakeAnalyzer)
library(dplyr)

great.temp <- load.ts("GreatPond2015.txt")
t.d <- ts.thermo.depth(great.temp)
t.d$diff <- c(NA, diff(t.d$thermo.depth))

dat <- read.csv("GreatPond2015_Chl.csv", skip = 0, header = TRUE, stringsAsFactors = FALSE)
t.d$chl <- dat$chl_2              
t.d$chldiff <- c(NA, diff(t.d$chl))

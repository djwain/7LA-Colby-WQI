# For each lake, compute correlation coefficient between zS and Ps, then plot vs lakes
# characteristics

library(tidyverse)
library(broom)
library(readxl)
library(ggplot2)

load("ME_daily_metrics.Rda")

dat <- WQ5 %>% filter(!is.na(zS_m),!is.na(Ps_ppb))

all_corr0 <- dat %>% 
  group_by(midas) %>% 
  do(model = lm(zS_m ~ Ps_ppb, data = .))

tmp0 <- sapply(all_corr0$model, coef)
tmp1 <- data.frame(t(tmp0) ) 

all_corr1 <- dat %>%
  group_by(midas) %>%
  do(glance(lm(zS_m ~ Ps_ppb, data = .)))

me_corr0 <- cbind(all_corr1, tmp1$Ps_ppb)
colnames(me_corr0)[14] <- "slope"

me_corr1 <- me_corr0 %>% filter(nobs > 10)

# Add morphological info
filepath <- "/Users/djw56/Dropbox (Personal)/Belgrade Lakes Stats/Maine Lake Data/"
filename <- paste(filepath,"MaineLakes_Geography_Morphometry.xls",sep="")
Morph <- read_excel(filename, sheet = 2)
colnames(Morph)[1] <- "midas"

me_corr2 <- merge(me_corr1,Morph)

ggplot(me_corr2, aes(x=`Flushing Rate (times/yr)`, y=slope)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 

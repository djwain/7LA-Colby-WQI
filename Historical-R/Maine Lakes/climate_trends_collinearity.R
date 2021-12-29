# Climate trends and collinearity
# DJW 4/28/21

library(tidyverse)
library(Kendall)


load("BL_climate.Rda")

dat <- climate %>% filter(Month == 4)
MannKendall(dat$AvgT)
MannKendall(dat$MaxT)
MannKendall(dat$MinT)
MannKendall(dat$Prec)
MannKendall(dat$CDD)
MannKendall(dat$HDD)
MannKendall(dat$PDSI)
MannKendall(dat$PHDI)
MannKendall(dat$PMDI)
MannKendall(dat$PZI)

write.csv(cor(climate),file="climate_corr.csv")

load("BL_ice.Rda")
dat <- ice %>% filter(Lake == "Messalonskee")
MannKendall(dat$YearDay)

load("BL_morph.Rda")
dat <- morph[,-1]
write.csv(cor(dat),file="morph_corr.csv")

# Check if average temperature is correlated with ice out date
dat0 <- climate[order(climate$Month),] 
dat1 <- dat0$AvgT
dat2 <- matrix(dat1, nrow = 126)
dat3 <- data.frame(cbind(unique(climate$Year),dat2))
colnames(dat3) <- c("Year","Apr","May","Jun","Jul","Aug","Sep","Oct")
write.csv(cor(dat3),file="temp_corr.csv")
  
dat4 <- ice #%>% filter(Lake == "Messalonskee")
dat4a <- dat4 %>% group_by(Year) %>% summarize(YD = mean(YearDay))
dat5 <- dat3 %>% filter(Year %in% dat4a$Year)
dat6 <- data.frame(cbind(dat5,dat4a$YD))
write.csv(cor(dat6),file="ice_corr.csv")

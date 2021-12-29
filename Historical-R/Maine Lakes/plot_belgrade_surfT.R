# Reads in the LSM data, computes the monthly average surface temperature
# for May - October(?), then averages that for a yearly average, then computes
# Also computes trends on August data.

library(readxl)
library(dplyr)
library(lubridate)
library(Kendall)
library(broom)

# Belgrade Lake MIDAS numbers 
BL_MIDAS <- c(5349, 5344, 5348, 5352, 5274, 5272, 5280)
BL <- c('EP','NP','MP','SL','GP','LP','ML')

# Find profiles from these MIDAS numbers
dat <- read_excel("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/MaineLakes_Temp_DO.xlsx", sheet = 2)
BL_TO <- dat %>%
  filter(MIDAS %in% BL_MIDAS)

# Get this working for one lake, then loop through others and get them on the same plot.
MIDAS1 <- 5349
TO <- BL_TO %>% filter(MIDAS == MIDAS1) 

# Filter by z <= 1 m
TO1 <- TO %>% filter(DEPTH <= 1)

# Average any points from the same day
Tday <- TO1 %>% group_by(Date) %>% summarise(T = mean(TEMPERATURE))
Tday$Month <- month(Tday$Date)
Tday$Year <- year(Tday$Date)

Tday %>% group_by(Month) %>% summarize(npts = n())

# Average data from the same month in the same year
Tmon <- aggregate(T~Month+Year, mean, data=Tday)

# Can't do summer averages because early data is quite spotty. Perhaps do it one month at a time
T1 <- Tmon %>% filter(Month >= 8 & Month <= 9 & Year > 1960)
Tsum <- T1 %>% group_by(Year) %>% summarize(npts = n(),yravg = mean(T))
Tsum <- Tsum %>% filter(npts >= 2)
mk<-MannKendall(Tsum$yravg)

# Determine linear fit, trendline and significance for each month
fitted_models <- Tmon %>% group_by(Month) %>% do(model = lm(T ~ Year, data = .))
output <- fitted_models %>% tidy(model) %>% filter(term == "Year")
output$CI2.5 <- output$estimate - 2*output$std.error
output$CI97.5 <- output$estimate + 2*output$std.error

# Do same for Summer AVerage
fitted_sum <- Tsum %>% do(model = lm(yravg ~ Year, data = .))
output2 <- fitted_sum %>% tidy(model) %>% filter(term == "Year")
output2$CI2.5 <- output2$estimate - 2*output2$std.error
output2$CI97.5 <- output2$estimate + 2*output2$std.error

plot(Tsum$Year,Tsum$yravg)

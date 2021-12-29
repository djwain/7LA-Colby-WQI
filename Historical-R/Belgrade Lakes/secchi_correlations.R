# Correlations – Use existing data to explore trends in key variables in relation to each other,
# including water clarity vs. surface P, bottom P, depth of anoxia, depth of oxygen <5 mg/L,
# depth of stratification, water temperature, precipitation, air temperature, and number of cloudy or
# sunny days. Characterize variation in conditions, allowing extrapolation of possible future range of,
# conditions in the lakes.
# DJW 21APR20

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)

lake <- 'East Pond'
site <- 'EPDEP1'

# Load data
folder <- paste("~/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/",lake,sep="","/Profiles/")
dat <- read_xlsx(paste(folder,site,sep=""," 2015-2019 Metrics.xlsx"))


# Plot secchi vs surface P
ggplot(dat, aes(x = Ps_ppb, y = zS_m)) +
  geom_point(aes(col=as.character(as.character(year))))+ 
  geom_smooth(method='lm') +
  labs(title = "Secchi vs Surf P", col = "Year")
dev.print(png, paste(folder,site,sep=""," Secchi vs Surf P.png"), width = 480 )


fit <- lm(dat$zS_m ~ dat$Ps_ppb)
summary(fit)
confint(fit, 'dat$Ps_ppb', level=0.95)
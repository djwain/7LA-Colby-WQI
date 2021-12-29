# McGrath Mystery!
# DJW 7/15/21

library(tidyverse)
library(broom)
library(readxl)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(party)
library(randomForest)
library(GGally)
library(ggparty)

load("ME_daily_metrics.Rda")


# Find shallow lakes that bloom
# Add morphological info
filepath <- "/Users/djw56/Dropbox (Personal)/Belgrade Lakes Stats/Maine Lake Data/"
filename <- paste(filepath,"MaineLakes_Geography_Morphometry.xls",sep="")
Morph <- read_excel(filename, sheet = 2)
colnames(Morph)[1] <- "midas"

max_depth <- 10*3.28 # depths are in ft

Morph1 <- Morph %>% 
  filter(`Depth_Max (feet)` < max_depth)

WQ6 <- WQ5 %>%
  group_by(midas) %>%
  summarize(zsmin = min(zS_m, na.rm = TRUE), zs01 = as.numeric(quantile(zS_m, c(0.01), na.rm = TRUE)))
WQ7 <- WQ6 %>% filter(!is.na(zsmin))

bloom <- merge(WQ7,Morph1,all= FALSE)
bloom$yn <- (bloom$zs01 < 2)
indxb <- which(bloom$yn == TRUE)
bloom$yn[indxb] <- "BLOOM"
indxnb <- which(bloom$yn == FALSE)
bloom$yn[indxnb] <- "ALL CLEAR"

indx <- which(is.na(bloom$`Total Drainage Area (sq miles)`))
bloom$`Total Drainage Area (sq miles)`[indx] <- bloom$`Direct Drainage Area (sq miles)`[indx]
bloom$DWALA <- bloom$`Direct Drainage Area (sq miles)`/(bloom$`Area (acres)`/640)
bloom$TWALA <- bloom$`Total Drainage Area (sq miles)`/(bloom$`Area (acres)`/640)

meanz_m <- bloom$`Depth_Mean (feet)`/3.28
A_km2 <- bloom$`Area (acres)`/247
bloom$OI <- meanz_m/sqrt(A_km2)

# grow tree
fit <- rpart(yn ~ `Area (acres)` + `Perimeter (miles)` + `Depth_Mean (feet)` 
             + `Depth_Max (feet)` + `Volume (acrefeet)` + `Direct Drainage Area (sq miles)`
             + `Total Drainage Area (sq miles)` + `Flushing Rate (times/yr)` + DWALA
             + TWALA + OI, method="class", data=bloom)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
rpart.plot(fit, uniform=TRUE,
     main="Bloom?")

bloom2 <- select(bloom, `Area (acres)`,`Perimeter (miles)`,`Depth_Mean (feet)`,
                 `Depth_Max (feet)`, `Volume (acrefeet)`, `Direct Drainage Area (sq miles)`,
                 `Total Drainage Area (sq miles)`,`Flushing Rate (times/yr)`,DWALA,
                 TWALA, OI, yn)
bloom2$yn <- as.factor(bloom2$yn)
fit2 <- ctree(yn ~ `Area (acres)` + `Perimeter (miles)` + `Depth_Mean (feet)` 
             + `Depth_Max (feet)` + `Volume (acrefeet)` + `Direct Drainage Area (sq miles)`
             + `Total Drainage Area (sq miles)` + `Flushing Rate (times/yr)` + DWALA
             + TWALA + OI, data=na.omit(bloom2))

names(bloom2) <- make.names(names(bloom2))
bloom2$Volume..acrefeet. <- bloom2$Area..acres.*bloom2$Depth_Mean..feet.
fit3 <- randomForest(yn ~ Area..acres. + Perimeter..miles. + Depth_Mean..feet. 
                     + Depth_Max..feet. + Volume..acrefeet. + Direct.Drainage.Area..sq.miles.
                     + Total.Drainage.Area..sq.miles. + Flushing.Rate..times.yr. + DWALA
                     + TWALA + OI, data=na.omit(bloom2))

fit4 <- rpart(yn ~ Depth_Max..feet. + Volume..acrefeet. + Total.Drainage.Area..sq.miles.
              + Flushing.Rate..times.yr. + OI, data=na.omit(bloom2))
rpart.plot(fit4, uniform=TRUE,
           main="Bloom?")
pfit<- prune(fit4, cp=   fit4$cptable[which.min(fit4$cptable[,"xerror"]),"CP"])
rpart.plot(pfit, uniform=TRUE,
           main="Bloom?")

fit5 <- ctree(yn ~ Depth_Max..feet. + Volume..acrefeet. + DWALA + OI, data=na.omit(bloom2))
rpart.plot(fit5, uniform=TRUE,
           main="Bloom?")
plot(fit5)


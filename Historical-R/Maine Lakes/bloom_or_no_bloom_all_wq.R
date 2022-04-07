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
library(caTools)

load("ME_daily_metrics.Rda")


# Find shallow lakes that bloom
# Add morphological info
filepath <- "/Users/djw56/Dropbox (Personal)/Belgrade Lakes Stats/Maine Lake Data/"
filename <- paste(filepath,"MaineLakes_Geography_Morphometry.xls",sep="")
Morph <- read_excel(filename, sheet = 2)
colnames(Morph)[1] <- "midas"



Morph1 <- Morph 

WQ6 <- WQ5 %>%
  group_by(midas) %>%
  summarize(zsmin = min(zS_m, na.rm = TRUE), 
            zs01 = as.numeric(quantile(zS_m, c(0.01), na.rm = TRUE)),
            Ts = median(Ts_C, na.rm = TRUE),
            Tb = median(Tb_C, na.rm = TRUE),
            zT = median(zT_m, na.rm = TRUE),
            zhypox = median(zhypox_m, na.rm = TRUE),
            zanox = median(zanox_m, na.rm = TRUE),
            Ps = median(Ps_ppb, na.rm = TRUE),
            Pb = median(Pb_ppb, na.rm = TRUE)
            )
            
WQ7 <- WQ6 %>% filter(!is.na(zs01))

bloom <- merge(WQ7,Morph1,all= FALSE)
bloom$yn <- (bloom$zs01 < 2)
indxb <- which(bloom$yn == TRUE)
bloom$yn[indxb] <- "BLOOM"
indxnb <- which(bloom$yn == FALSE)
bloom$yn[indxnb] <- "ALL CLEAR"
bloom$yn <- as.factor(bloom$yn)

names(bloom) <- make.names(names(bloom))
indx <- which(is.na(bloom$Total.Drainage.Area..sq.miles.))
bloom$Total.Drainage.Area..sq.miles.[indx] <- bloom$Direct.Drainage.Area..sq.miles.[indx]
bloom$DWALA <- bloom$Direct.Drainage.Area..sq.miles./(bloom$Area..acres./640)
bloom$TWALA <- bloom$Total.Drainage.Area..sq.miles./(bloom$Area..acres./640)

meanz_m <- bloom$Depth_Mean..feet./3.28
A_km2 <- bloom$Area..acres./247
bloom$OI <- meanz_m/sqrt(A_km2)
bloom$Volume..acrefeet. <- bloom$Area..acres.*bloom$Depth_Mean..feet.

# grow tree with rpart
fit0 <- rpart(yn ~ Area..acres. + Perimeter..miles. + Depth_Mean..feet. 
              + Depth_Max..feet. + Volume..acrefeet. + Direct.Drainage.Area..sq.miles.
              + Total.Drainage.Area..sq.miles. + Flushing.Rate..times.yr. + DWALA
              + TWALA + OI + Ts + Tb + zT + zhypox +zanox + Ps +Pb, data=na.omit(bloom))

printcp(fit0) # display the results
plotcp(fit0) # visualize cross-validation results
summary(fit0) # detailed summary of splits
rpart.plot(fit0, uniform=TRUE,
     main="Bloom?")
pfit0<- prune(fit0, cp=   fit0$cptable[which.min(fit0$cptable[,"xerror"]),"CP"])
rpart.plot(pfit0, uniform=TRUE,
           main="Bloom?")


bloom2 <- select(bloom, Area..acres., Perimeter..miles., Depth_Mean..feet., 
                 Depth_Max..feet., Volume..acrefeet., Direct.Drainage.Area..sq.miles.,
                 Total.Drainage.Area..sq.miles., Flushing.Rate..times.yr., DWALA, TWALA, OI,
                 Ts, Tb, zT, zhypox, zanox, Ps, Pb, yn)
fit1 <- ctree(yn ~ Area..acres. + Perimeter..miles. + Depth_Mean..feet. 
              + Depth_Max..feet. + Volume..acrefeet. + Direct.Drainage.Area..sq.miles.
              + Total.Drainage.Area..sq.miles. + Flushing.Rate..times.yr. + DWALA
              + TWALA + OI + Ts + Tb + zT + zhypox +zanox + Ps +Pb, data=na.omit(bloom2))
plot(fit1)

fit2 <- randomForest(yn ~ Area..acres. + Perimeter..miles. + Depth_Mean..feet. 
                     + Depth_Max..feet. + Volume..acrefeet. + Direct.Drainage.Area..sq.miles.
                     + Total.Drainage.Area..sq.miles. + Flushing.Rate..times.yr. + DWALA
                     + TWALA + OI + Ts + Tb + zT + zhypox +zanox + Ps +Pb, data=na.omit(bloom2))

sample <- sample.split(bloom2$yn)
train <- subset(bloom2, sample == TRUE)
test  = subset(bloom2, sample == FALSE)
fit3 <- randomForest(yn ~ Area..acres. + Perimeter..miles. + Depth_Mean..feet. 
                    + Depth_Max..feet. + Volume..acrefeet. + Direct.Drainage.Area..sq.miles.
                    + Total.Drainage.Area..sq.miles. + Flushing.Rate..times.yr. + DWALA
                    + TWALA + OI + Ps, data=na.omit(train))
pred = predict(fit3, newdata=test[-19])
cm = table(test[,19], pred)
importance(fit3)

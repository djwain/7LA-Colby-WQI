# Daily average Goldie data
# DJW 3/8/21

library(dplyr)
library(tidyverse)


# Load data
inname <- "GoldieCleanTrim2015.csv"
outname <- "GreatPond2015.txt"
Goldie <- read.csv(inname, skip = 0, header = TRUE, stringsAsFactors = FALSE)

# Isolate data for grouping
Goldie$datetime<- as.Date(Goldie$Date.Time)

# Compute daily average
dayavg <- Goldie %>% group_by(datetime) %>% summarize_all("mean",na.rm = TRUE)

# Restructure data for GLEON Mixing Project
dat <- select(dayavg,"datetime","X1m.T","X3m.T","X5m.T","X7m.T",
              "X9m.T","X11m.T","X13m.T","X15m.T","X17m.T","X19m.T")
dat1 <- dat %>% rename(wtr_1 = X1m.T,wtr_3 = X3m.T,wtr_5 = X5m.T,wtr_7 = X7m.T,
               wtr_9 = X9m.T,wtr_11 = X11m.T,wtr_13 = X13m.T,wtr_15 = X15m.T,
               wtr_17 = X17m.T,wtr_19 = X19m.T)
dat1[,-1] <-round(dat1[,-1],2)

write.table(dat1, outname,sep = '\t', row.names=FALSE) 

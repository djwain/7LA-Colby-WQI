library(stringr)
library(gridExtra)
library(readxl)
library(dplyr)
library(tidyr)
library(naniar)

# This script trims the Goldie data using the thermistors to determine when it was
# out of the water by checking when there is a stable T gradient

dat <- "GoldieClean2019.csv"
Goldie <- read.csv(dat, skip = 0, header = TRUE, stringsAsFactors = FALSE)

GoldieT <- select(Goldie,
                  X1m.T,X3m.T,X5m.T,X7m.T,X9m.T,X11m.T,X13m.T,X15m.T,X17m.T,X19m.T)
GoldieT[GoldieT < 0] <- 100
GoldieT <- as.matrix(as.data.frame(lapply(GoldieT, as.numeric)))

# Create a matrix of T19 values to use in matrix calcs
GoldieT19 <- cbind(GoldieT[,10],GoldieT[,10],GoldieT[,10],GoldieT[,10],GoldieT[,10],GoldieT[,10],GoldieT[,10],GoldieT[,10],GoldieT[,10],GoldieT[,10])
delT19 = GoldieT-GoldieT19

# Find rows with no negatives in delT
has.neg <- apply(delT19, 1, function(row) any(row < 0))
pos.ind <- which(!has.neg)

# Remove these rows
Goldie2 <- Goldie[first(pos.ind):last(pos.ind),-c(1)]
GoldieT2 <- select(Goldie2,
                  X1m.T,X3m.T,X5m.T,X7m.T,X9m.T,X11m.T,X13m.T,X15m.T,X17m.T,X19m.T)
GoldieT[GoldieT > 99] <- -100
GoldieT2 <- as.matrix(as.data.frame(lapply(GoldieT2, as.numeric)))

# Create a matrix of T1 values to use in matrix calcs
GoldieT1 <- cbind(GoldieT2[,1],GoldieT2[,1],GoldieT2[,1],GoldieT2[,1],GoldieT2[,1],GoldieT2[,1],GoldieT2[,1],GoldieT2[,1],GoldieT2[,1],GoldieT2[,1])
delT1 = GoldieT1-GoldieT2

# Find rows with no negatives in delT
has.neg1 <- apply(delT1, 1, function(row) any(row < 0))
pos.ind1 <- which(!has.neg1)

# Trim rows
Goldie3 <- Goldie2[first(pos.ind1):last(pos.ind1),]
write.csv(Goldie3, file = "GoldieCleanTrim2019.csv") 


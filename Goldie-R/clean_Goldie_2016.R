library(stringr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)
library(tidyr)

    filedir <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Great Pond/RawData/Goldie/"
    filename <- "Goldie2016.csv"
    dat <- str_c(filedir,filename)
    
    # Goldie data has a weird header structure that includes a greek symbol that crashes R.
    # First I am loading it as chr, removing the weird lines, then resaving
    Goldie <- read.csv(dat, skip = 0, header = FALSE, stringsAsFactors = FALSE)
    Goldie <- Goldie[-c(1:2),]
    # Get rid of the Greek symbols because it messes things up and rename columns so consistent between years
    Goldie[1,11] <- '1m T'
    Goldie[1,12] <- '3m T'
    Goldie[1,13] <- '15m T'
    Goldie[1,14] <- '13m T'
    Goldie[1,15] <- '11m T'
    Goldie[1,16] <- '9m T'
    Goldie[1,17] <- '5m T'
    Goldie[1,18] <- '7m T'
    Goldie[1,19] <- '17m T'
    Goldie[1,20] <- '19m T'
    Goldie[1,21] <- 'Underwater PAR'
    Goldie[1,22] <- 'Surface PAR'
    Goldie[1,25] <- '6m DO'
    Goldie[1,28] <- '9.6m DO'
    Goldie[1,30] <- '6m Chlorophyll'
    Goldie[2,4] <- 'ug/L'
    Goldie[2,21] <- 'umol/s/m2'
    Goldie[2,22] <- 'umol/s/m2'
    Goldie[2,30] <- 'ug/L'
    write.csv(Goldie, file = "GoldieCleanHeader.csv")

    dat <- "GoldieCleanHeader.csv"
    Goldie <- read.csv(dat, skip = 1, header = TRUE, stringsAsFactors = FALSE)

    # Convert date column to something useful
    Goldie$Date.Time <- as.POSIXct(Goldie$Date.Time, format = "%m/%d/%Y %I:%M:%S %p")
    
    # Get rid of bad data lines. Use chlorophyll = -99999.99 and T < 0
    Goldie1 <- filter(Goldie, X2m.Chlorophyll.mv >= 0)
    Goldie2 <- filter(Goldie1,X1m.T > 0)
    
    # Remove unuseful columns (bat, Chl in mv)
    Goldie3 <- select(Goldie2,Date.Time,Underwater.PAR,Surface.PAR,
                      X2m.Chlorophyll,X6m.Chlorophyll,
                      X2m.DO,X6m.DO,X9.6m.DO,X16m.DO,
                      X1m.T,X3m.T,X5m.T,X7m.T,X9m.T,X11m.T,X13m.T,X15m.T,X17m.T,X19m.T)
    write.csv(Goldie3, file = "GoldieClean2016.csv")    
    





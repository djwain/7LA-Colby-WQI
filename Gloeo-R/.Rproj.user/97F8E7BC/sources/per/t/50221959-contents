# This script makes a box plot of the all the data from Great Pond for each year.
# DJW 12FEB20

library(stringr)
library(ggplot2)
library(dplyr)

filedir <- "/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Great Pond/RawData/CitizenScience/"
filename <- "GP Gloeo.csv"
dat <- str_c(filedir,filename)

Gloeo <- read.csv(dat, skip = 0, header = TRUE, stringsAsFactors = FALSE)
Gloeo$Year <- as.character(Gloeo$Year)

p <- ggplot(data = Gloeo, aes(x = Year,y = Density, fill = Site)) +
  geom_boxplot() +
  labs(title = "Great Pond Gloeo")

print(p)
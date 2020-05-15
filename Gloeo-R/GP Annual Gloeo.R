# This script makes a box plot of the all the data from Great Pond for each year.
# DJW 12FEB20

# Load packages
library(stringr)
library(ggplot2)
library(dplyr)

# Load file
filename <- "GP Gloeo.csv"
dat <- str_c(filename)

# Convert year to string for box plot
Gloeo <- read.csv(dat, skip = 0, header = TRUE, stringsAsFactors = FALSE)
Gloeo$Year <- as.character(Gloeo$Year)

# Make box plot by year and by site
p <- ggplot(data = Gloeo, aes(x = Year,y = Density, fill = Site)) +
  geom_boxplot() +
  labs(title = "Great Pond Gloeo")
print(p)
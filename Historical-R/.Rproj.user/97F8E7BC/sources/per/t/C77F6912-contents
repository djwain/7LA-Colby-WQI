# This script plots the ice out date for all of the Belgrade Lakes from the LSM Data
# DJW 06APR20

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)

# Belgrade Lake MIDAS numbers 
BL_MIDAS <- c(5272, 5274, 5280, 5344, 5348, 5349, 5352)
BL <- c('LP','GP','ML','NP','MP','EP','SL')

# Find ice off data from these MIDAS numbers
Ice <- read.csv("/Users/djw56/Documents/Research/7LA-Colby/Belgrade Lakes/Lakes/Belgrades/Historical/Ice Data Spreadsheet combined v2.csv", header = TRUE, stringsAsFactors = FALSE)
BL_Ice <- Ice %>%
  filter(MIDAS %in% BL_MIDAS)

# Convert ice out date to Year Day
a <- mdy(BL_Ice$Ice.Out)
BL_Ice$Year <- year(a)
BL_Ice$YearDay <- yday(a)

# Plot ice off dates for each lake in different colors
ggplot(BL_Ice, aes(x = Year, y = YearDay)) + 
  geom_point(aes(col=as.character(as.character(MIDAS))))+
  geom_smooth(method='lm') +
  labs(title = "Ice Off Date", y = "Year Day", x = "Year", col = "Lake") +
  scale_color_hue(labels = BL)


NP_Ice <- BL_Ice %>% filter(MIDAS == 5344)
ggplot(NP_Ice, aes(x = Year, y = YearDay)) + 
  geom_point()+
  geom_smooth(method='lm') +
  labs(title = "Ice Off Date", y = "Year Day", x = "Year")

# Determine linear fit, trendline and significance for each month
fitted_models <- NP_Ice %>% do(model = lm(YearDay ~ Year, data = .))
output <- fitted_models %>% tidy(model) %>% filter(term == "Year")
output$CI2.5 <- output$estimate - 2*output$std.error
output$CI97.5 <- output$estimate + 2*output$std.error

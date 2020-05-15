# This script plots the average temperature for each month for May - September for 
# Maine Climate District 2. It also plots the average of these averages for each year
# DJW 4/9/20

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(broom)


theme_set(theme_bw()) 


# Find data
datapath <- "~/Documents/Research/7LA-Colby/Belgrade Lakes/Climate/"
filenames <- dir(datapath, pattern="1702-tavg-1-")

# Combine into one data frame for manipulation and plotting
Tmonthlyavg <- ""
for(i in 1:length(filenames)){

fullpath <- paste(datapath,filenames[i],sep = "")
dat <- read.csv(fullpath,skip = 4,header = TRUE, stringsAsFactors = FALSE)

# Parse data into year and month columns
dat$Year <- substr(dat$Date,1,4)
dat$Month <- substr(dat$Date,6,6)
#dat$Month <- month.abb[as.numeric(substr(dat$Date,5,6))]

# Convert to Celcius
dat$TC <- (dat$Value-32)*5/9

# Not happy that this seems to turn everything to characters but can't
# figure out why - just have to remember to convert back outside afterwards
Tmonthlyavg <- rbind.data.frame(Tmonthlyavg, dat)
}

Tmonthlyavg <- Tmonthlyavg[-c(1),]
Tmonthlyavg$TC <- as.numeric(Tmonthlyavg$TC)
Tmonthlyavg$Year <- as.numeric(Tmonthlyavg$Year)

test <- Tmonthlyavg %>% group_by(Year) %>% summarize(summeravg = mean(TC))
test2 <- Tmonthlyavg %>% filter(Month == 7) %>% 
  group_by(Year) %>% summarize(JJAavg = mean(TC))

# Plot avg temp data with each month a different color
Months <- c("May","Jun","Jul","Aug","Sep")
ggplot(Tmonthlyavg, aes(Year, TC, color=Month, fill=Month)) + 
  geom_point() +
  geom_smooth(method="lm") +
  labs(title = "Average Monthly Temperature", y = "Temperature / C", x = "Year")

# Determine linear fit, trendline and significance for each month
fitted_models <- Tmonthlyavg %>% group_by(Month) %>% do(model = lm(TC ~ Year, data = .))
output <- fitted_models %>% tidy(model) %>% filter(term == "Year")
output$CI2.5 <- output$estimate - 2*output$std.error
output$CI97.5 <- output$estimate + 2*output$std.error

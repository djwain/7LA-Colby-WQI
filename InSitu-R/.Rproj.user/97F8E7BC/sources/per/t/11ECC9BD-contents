# This is just a quick script to show there is no significant trend in the anoxic factor
# over 2015-2019
# DJW 21APR20

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# Data
years <- c(1989, 1995, 1998, 2003, 2009, 2015:2019)
AF <- c(0.163,
        0.705,
        2.457,
        5.318,
        2.617,
        8.525,
5.714,
9.821,
7.344,
11.754)
iceout <- c(116, 106, 100, 117, 102, 114, 89, 113, 112, 113)
meantemp <- c(16.4, 16.2, 16.9, 16.6, 15.8, 17.2, 17.1, 16.7, 17.4, 16.1)
JJA <- c(18.2, 19.3, 18.4, 19.0, 17.8, 18.1, 19.1, 18.3, 19.4, 18.8)
Jul <- c(19.4, 20.8, 19.9, 20.0, 17.9, 19.1, 20.5, 19.4, 21.2, 21.1)
Aug <- c(18.2, 19.2, 19.4, 19.9, 19.9, 20.3, 20.2, 18.5, 20.9, 19.1)
Tbot <- c(9.3,10.5,10.7, 9.4,11.7,8.7,10.6,9.7,9.1,10.1)


dat <- data.frame(years, AF, iceout, meantemp, JJA, Jul, Aug, Tbot)

# Plot anoxic factor vs year
ggplot(dat, aes(x = years, y = AF)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "GP Anoxic Factor", y = "AF (d/y)", x = "Year")

fit <- lm(dat$AF ~ dat$years)
summary(fit)
confint(fit, 'dat$years', level=0.95)

# Plot anoxic factor vs ice out date
ggplot(dat, aes(x = iceout, y = AF)) +
        geom_point() +
        geom_smooth(method='lm') +
        labs(title = "GP Anoxic Factor", y = "AF (d/y)", x = "Ice Out Year Day")

fit <- lm(dat$AF ~ dat$iceout)
summary(fit)
confint(fit, 'dat$iceout', level=0.95)

# Plot anoxic factor vs May-Sept mean temp
ggplot(dat, aes(x = meantemp, y = AF)) +
        geom_point() +
        geom_smooth(method='lm') +
        labs(title = "GP Anoxic Factor", y = "AF (d/y)", x = "May - Sep Mean Temp (C)")

fit <- lm(dat$AF ~ dat$meantemp)
summary(fit)
confint(fit, 'dat$meantemp', level=0.95)

# Plot anoxic factor vs JJA mean temp
ggplot(dat, aes(x = JJA, y = AF)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "GP Anoxic Factor", y = "AF (d/y)", x = "Jun - Aug Mean Temp (C)")

fit <- lm(dat$AF ~ dat$JJA)
summary(fit)
confint(fit, 'dat$JJA', level=0.95)

# Plot anoxic factor vs July mean temp
ggplot(dat, aes(x = Jul, y = AF)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "GP Anoxic Factor", y = "AF (d/y)", x = "July Mean Temp (C)")

fit <- lm(dat$AF ~ dat$Jul)
summary(fit)
confint(fit, 'dat$Jul', level=0.95)

# Plot anoxic factor vs Aug mean temp
ggplot(dat, aes(x = Aug, y = AF)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "GP Anoxic Factor", y = "AF (d/y)", x = "August Mean Temp (C)")

fit <- lm(dat$AF ~ dat$Aug)
summary(fit)
confint(fit, 'dat$Aug', level=0.95)

# Plot anoxic factor vs mean bottom temp
ggplot(dat, aes(x = Tbot, y = AF)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "GP Anoxic Factor", y = "AF (d/y)", x = "T Bottom (C)")

fit <- lm(dat$AF ~ dat$Tbot)
summary(fit)
confint(fit, 'dat$Tbot', level=0.95)

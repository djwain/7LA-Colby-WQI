library(stringr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)

theme_set(theme_bw()) 
theme(plot.title = element_text(hjust = 0.5))

site <- 'epdep1'
sec <- read_xlsx("data/secchi_2019.xlsx", sheet = site)
secft <- sec$'Depth(m)'*3.28

p1 <- ggplot(sec, aes(sec$Date,secft))+
  geom_point()+
  scale_y_reverse(limits = c(maxdep, 0)) +
  labs(y = "Depth (ft)",
       x = "Date",
       title = 2019)


dat <- "data/EPDEP1_2019-09-30_12-21-37_log.csv"

junk <- read.csv(dat, header = FALSE, stringsAsFactors = FALSE)
if(str_detect(junk,"GPS") == TRUE) {
  skipno = 10
}else {
  skipno = 9
}



T.prof <- read.csv(dat, skip = skipno, header = TRUE, stringsAsFactors = FALSE)
#convert to imperial system units
meters <- T.prof[['Depth..m.']]
feet <- meters*3.28
Cels <- T.prof[['Temp..C.']]
Fahr <- (Cels*9/5)+32
T.prof$Depth.ft <- feet
T.prof$Temp.F <- Fahr
maxdepi <-which.max(T.prof$Depth.ft)
maxdep <- T.prof$Depth.ft[maxdepi]
Dep2 <- T.prof$Depth.ft[1:maxdepi]
Temp2 <- T.prof$Temp.F[1:maxdepi]
O22<- T.prof$RDO.Sat....[1:maxdepi]
datestr <- str_sub(T.prof$Created[1],1,10)


p1 <- ggplot(T.prof[1:maxdepi,], aes(T.prof$Temp.F[1:maxdepi], T.prof$Depth.ft[1:maxdepi]))+
geom_point()+
  scale_y_reverse(limits = c(NA, 0)) +
  labs(y = "Depth (ft)",
       x = "Temperature (F)",
       title = datestr)
  



p2 <- ggplot(T.prof[1:maxdepi,], aes(x=T.prof$RDO.Sat....[1:maxdepi], y=T.prof$Depth.ft[1:maxdepi]))+
geom_point()+
  labs(y = "Depth (ft)",
       x = "% Dissolved Oxygen",
       title = datestr,
       caption = "If this is app is broken, contact danielle.wain at 7lakesalliance.org")+
  scale_y_reverse(limits = c(NA, 0))

grid.arrange(p1, p2, nrow = 2)

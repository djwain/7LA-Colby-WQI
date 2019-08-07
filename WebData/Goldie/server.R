library(stringr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)
library(tidyr)

theme_set(theme_bw()) 



server <- function(input, output) {
  
  output$timeseries <- renderPlot({
    timeint <- switch(input$range,
                  day = 24*4,
                  week = 7*24*4,
                  month = 30*24*4
    )
    
    
    
    
    dat <- "data/Goldie2017.csv"
    
    # Goldie data has a weird header structure that includes a greek symbol that crashes R.
    # First I am loading it as chr, removing the weird lines, then resaving
    Goldie <- read.csv(dat, skip = 0, header = FALSE, stringsAsFactors = FALSE)
    Goldie <- Goldie[-c(1:2),]
    Goldie <- Goldie[-c(2),]
    write.csv(Goldie, file = "data/CleanGoldie.csv")
    
    dat <- "data/CleanGoldie.csv"
    Goldie <- read.csv(dat, skip = 1, header = TRUE, stringsAsFactors = FALSE)
    
    # Convert date column to something useful
    Goldie$Date.Time <- as.POSIXct(Goldie$Date.Time, format = "%m/%d/%Y %I:%M:%S %p")
    
    # Now lets plot the last 24 hours of data
    G24 <- tail(Goldie,timeint)
    
    # Plot temperature
    T24 <- select(G24,Date.Time,X1.m.T,X3m.T,X5m.T,X7m.T,X9m.T,X11m.T,X13m.T,X15m.T,X17m.T,X19m.T) 
    names(T24) <- c("Date","03ft","10ft","16ft","23ft","30ft","36ft","43ft","49ft","56ft","62ft")
    T24long <- gather(T24,Depth,temp,"03ft","10ft","16ft","23ft","30ft","36ft","43ft","49ft","56ft","62ft")
    T24long[T24long < 0] <- NA
    
    p1 <- ggplot(T24long, aes(x = Date, y = (temp*9/5+32), colour = Depth))+
      geom_line() +
      labs(y = "Temperature (F)", x = "Date")
    
    # Plot oxygen
    DO24 <- select(G24,Date.Time,X2m.DO.,X6m.DO.sat,X16m.DO.) 
    names(DO24) <- c("Date","07ft","20ft","52ft")
    DO24long <- gather(DO24,Depth,DO,"07ft","20ft","52ft")
    p2 <- ggplot(DO24long, aes(x = Date, y = DO, colour = Depth))+
      geom_line() +
      labs(y = "Dissolved Oxygen (% Saturation)", x = "Date")
    
    grid.arrange(p1, p2, nrow = 1)
    
         
  })
}


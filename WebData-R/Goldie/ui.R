library(stringr)
library(ggplot2)
library(gridExtra)
library(readxl)
library(dplyr)
library(tidyr)

theme_set(theme_bw()) 

ui <- fluidPage(
  titlePanel("Great Pond Buoy Data"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("range", 
        "Choose a time frame:", 
        c("Last 24 hrs" = "day",
          "Last 7 days" = "week",
          "Last 30 days" = "month"))),
    mainPanel(img(src = "goldie.jpg", height = 0.1*3024, width = 0.1*4032))),
  plotOutput("timeseries")
)


library(shiny)
library(stringr)
library(ggplot2)
library(gridExtra)
library(readxl)

theme_set(theme_bw()) 

ui <- fluidPage(
  titlePanel("Belgrade Lakes Water Quality"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("lake", 
                   "Choose a lake:", 
                   c("East Pond" = "epdep1",
                     "Great Pond" = "gpdep2",
                     "Long Pond (Lower)" = "lpdep2",
                     "Long Pond (Upper)" = "lpdep1",
                     "McGrath Pond" = "mpdep1",
                     "Messalonskee Lake" = "messdep1",
                     "North Pond" = "npdep1",
                     "Salmon Lake" = "spdep1"))),
    
    mainPanel(img(src = "BelgradeLakes.png", height = 0.2*1372, width = 0.2*820))),
  plotOutput("profiles")
  #uiOutput("lake")
)

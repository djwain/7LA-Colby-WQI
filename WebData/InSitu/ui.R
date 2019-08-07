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
                   c("East Pond" = "ep",
                     "Great Pond" = "gp",
                     "Long Pond (Lower)" = "lpl",
                     "Long Pond (Upper)" = "lpu",
                     "McGrath Pond" = "mp",
                     "Messalonskee Lake" = "ml",
                     "North Pond" = "np",
                     "Salmon Lake" = "sl"))),
    mainPanel(img(src = "BelgradeLakes.png", height = 0.3*1372, width = 0.3*820))),
  plotOutput("profiles")
)

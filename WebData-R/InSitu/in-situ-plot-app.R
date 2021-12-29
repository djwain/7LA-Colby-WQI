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

server <- function(input, output, session) {
  
  
  
  
  # output$lake <- renderUI({
  #   pwd <- getwd()
  #   filenames <- dir(paste(pwd,'data',sep = "/"), pattern = input$lake)
  #   radioButtons("lake","Choose a Date:", c = filenames)
  # })
  
  output$profiles <- renderPlot({
    
  
    site <- input$lake
    sec <- read_xlsx("data/secchi_2019.xlsx", sheet = site)

    dat <- switch(input$lake,
                  epdep1 = "data/EPDEP1_2020-07-16_16-23-50_log.csv",
                  gpdep2 = "data/GPDEP1_2020-07-16_12-11-03_log.csv",
                  lpdep2 = "data/LPDEP2_2020-07-04_16-51-17_log.csv",
                  lpdep1 = "data/LPDEP1_2020-07-05_16-08-40_log.csv",
                  mpdep1 = "data/MPDEP1_2020-07-04_10-14-21_log.csv",
                  messdep1 = "data/MESSDEP1_2020-07-15_15-40-14_log.csv",
                  npdep1 = "data/NPDEP1_2020-07-03_10-35-54_log.csv",
                  spdep1 = "data/SPDEP1_2020-07-04_09-41-11_log.csv"
                  )
    
    # The number of header lines is different depending on which device the measurement was taken on.
    # The difference is the GPS line, so using that as indicator
    suppressWarnings({
    junk <- read.csv(dat, header = FALSE, stringsAsFactors = FALSE)
    if(str_detect(junk,"GPS") == TRUE) {
      skipno = 10
    }else {
      skipno = 9
    }
    })

    T.prof <- read.csv(dat, skip = skipno, header = TRUE, stringsAsFactors = FALSE)
    #convert to imperial system units
    meters <- T.prof[['Depth..m.']]
    feet <- meters*3.28
    Cels <- T.prof[['Temp..C.']]
    Fahr <- (Cels*9/5)+32
    T.prof$Depth.ft <- feet
    T.prof$Temp.F <- Fahr
    maxdepi <-which.max(T.prof$Depth.ft)
    
    Dep2 <- T.prof$Depth.ft[1:maxdepi]
    Temp2 <- T.prof$Temp.F[1:maxdepi]
    O22<- T.prof$RDO.Sat....[1:maxdepi]
    datestr <- str_sub(T.prof$Created[1],1,10)
    
    secft <- sec$'Depth(m)'*3.28
    
    # p1 <- ggplot(sec, aes(sec$Date,secft))+
    #                geom_point()+
    #                scale_y_reverse(limits = c(max(Dep2), 0)) +
    #                labs(y = "Depth (ft)",
    #                     x = "Date",
    #                     title = "Secchi 2019")
    
    p2 <- ggplot(T.prof[1:maxdepi,], aes(T.prof$Temp.F[1:maxdepi], T.prof$Depth.ft[1:maxdepi]))+
      geom_point()+
      scale_y_reverse(limits = c(NA, 0)) +
      labs(y = "Depth (ft)",
           x = "Temperature (F)",
           title = datestr)
    
    p3 <- ggplot(T.prof[1:maxdepi,], aes(T.prof$RDO.Sat....[1:maxdepi], T.prof$Depth.ft[1:maxdepi]))+
      geom_point()+
      labs(y = "Depth (ft)",
           x = "% Dissolved Oxygen",
           title = datestr,
           caption = "If this is app is broken or you have suggestions to improve it, contact danielle.wain at 7lakesalliance.org")+
      scale_y_reverse(limits = c(NA, 0))
      
    
    #grid.arrange(p1, p2, p3, nrow = 3)
    grid.arrange(p2, p3, nrow = 2)
         
  }, width = 470, height = 470*2)

  HTML("<br><br><br>")
} 

shinyApp(ui = ui, server = server)
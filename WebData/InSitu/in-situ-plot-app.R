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

server <- function(input, output) {
  
  output$profiles <- renderPlot({
    dat <- switch(input$lake,
                  ep = "data/ep_most_recent.csv",
                  gp = "data/gp_most_recent.csv",
                  lpl = "data/lpl_most_recent.csv",
                  lpu = "data/lpu_most_recent.csv",
                  mp = "data/mp_most_recent.csv",
                  ml = "data/ml_most_recent.csv",
                  np = "data/np_most_recent.csv",
                  sl = "data/sl_most_recent.csv"
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
    
    
    p1 <- ggplot(T.prof[1:maxdepi,], aes(T.prof$Temp.F[1:maxdepi], T.prof$Depth.ft[1:maxdepi]))+
      geom_point()+
      scale_y_reverse(limits = c(NA, 0)) +
      labs(y = "Depth (ft)",
           x = "Temperature (F)",
           title = datestr)
    
    p2 <- ggplot(T.prof[1:maxdepi,], aes(T.prof$RDO.Sat....[1:maxdepi], T.prof$Depth.ft[1:maxdepi]))+
      geom_point()+
      labs(y = "Depth (ft)",
           x = "% Dissolved Oxygen",
           title = datestr)+
      scale_y_reverse(limits = c(NA, 0))
    
    grid.arrange(p1, p2, nrow = 1)
         
  })
}

shinyApp(ui = ui, server = server)
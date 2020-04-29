library(tidyverse)
library(shiny)

#READ IN DATA
pitchFraming <- read_csv("/Users/benstarks/Desktop/Baseball/Master_Copy_W_Catchers.csv")
pitchFraming <- pitchFraming[pitchFraming$PitcherTeam == "GEO_BUL",] #only Georgia catchers


#FUNCTION TO CREATE THE PLOT
calculate <- function(catcher, pitcher, pitchType){
  
  #filter data
  pitchFraming <- pitchFraming[pitchFraming$PitchCall == "BallCalled" | pitchFraming$PitchCall == "StrikeCalled",] #only called pitches
  pitchFraming <- pitchFraming[pitchFraming$PitcherTeam == "GEO_BUL",] #only Georgia catchers
  if(catcher != "ALL"){
    pitchFraming <- pitchFraming[pitchFraming$Catcher == catcher,] #filter by catcher if applicable
  }
  if(pitcher != "ALL"){
    pitchFraming <- pitchFraming[pitchFraming$Pitcher == pitcher,] #filter by pitcher if applicable
  }
  if(pitchType != "ALL"){
    pitchFraming <- pitchFraming[pitchFraming$TaggedPitchType == pitchType,] #filter by pitchType if applicable
  }
  
  #calculate good/bad calls
  pitchFraming$realStrike <- ifelse((pitchFraming$PlateLocHeight>=1.6 & pitchFraming$PlateLocHeight <= 3.5 & pitchFraming$PlateLocSide >= -0.95 & pitchFraming$PlateLocSide <= 0.95), 1, 0)
  pitchFraming$strikeCalledBall <- ifelse((pitchFraming$realStrike == 1 & pitchFraming$PitchCall == 'BallCalled'), 1, 0)
  pitchFraming$ballCalledStrike <- ifelse((pitchFraming$realStrike == 0 & pitchFraming$PitchCall == 'StrikeCalled'), 1, 0)
  pitchFraming$goodCall <- ifelse((pitchFraming$realStrike == 0 & pitchFraming$PitchCall == "BallCalled") | (pitchFraming$realStrike == 1 & pitchFraming$PitchCall == "StrikeCalled"), 1, 0)
  pitchFraming$color <- ifelse(pitchFraming$goodCall == 1, 'Good Call', ifelse(pitchFraming$strikeCalledBall == 1, 'Strike Called Ball', 'Ball Called Strike'))
  
  #define strikezone
  x <- c(-.95,.95,.95,-.95,-.95)
  z <- c(1.6,1.6,3.5,3.5,1.6)
  sz <- data_frame(x,z) 
  
  #create strikezone graph
  title1 <- paste0(catcher," Pitch Framing")
  ggplot()+
    geom_path(data = sz, aes(x=x, y=z))+
    coord_equal()+
    ggtitle(title1)+
    xlab("Plate Location Side")+
    ylab("Plate Location Height")+
    geom_point(
      data = pitchFraming,aes(x=PlateLocSide,y=PlateLocHeight,color = color, alpha = color))+
    scale_color_manual(
      values = c("Good Call" = "black", "Ball Called Strike" = "green", "Strike Called Ball" = "red"),
      name = "Key")+
    scale_alpha_manual(
      values = c("Good Call" = 0.1, "Ball Called Strike" = 0.5, "Strike Called Ball" = 0.5), guide = FALSE)
}


#FUNCTION TO DISPLAY THE PLOT
server <- function(input, output){
  
  output$catcherPlot <- renderPlot({calculate(input$catcher, input$pitcher, input$pitchType)})
}


#CREATES THE CHOICES FOR THE USER
catcherNames <- unique(pitchFraming$Catcher)
catcherNames <- append(catcherNames, "ALL")
catcherNames <- catcherNames[!is.na(catcherNames)]
pitcherNames <- unique(pitchFraming$Pitcher)
pitcherNames <- append(pitcherNames, "ALL")
pitchTypeNames <- unique(pitchFraming$TaggedPitchType)
pitchTypeNames <- append(pitchTypeNames, "ALL")


#CREATES THE USER INTERFACE
ui <- fluidPage(
  
  #Title
  titlePanel("UGA Pitch Framing"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: Selector for variables
      selectInput("catcher", "Catcher:", 
                  choices=catcherNames,
                  selected = "ALL"),
      selectInput("pitcher", "Pitcher:", 
                  choices=pitcherNames,
                  selected = "ALL"),
      selectInput("pitchType", "Pitch Type:", 
                  choices=pitchTypeNames,
                  selected = "ALL")
    ),
    # Output: Pitch Framing plot (created in calculate function)
    mainPanel(
      plotOutput("catcherPlot")
    )
  )
)

shinyApp(ui, server)


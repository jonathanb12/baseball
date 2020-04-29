
library(dplyr)
library(ggplot2)
pitchFraming <- read_csv("/Users/jonathanbell/Desktop/Baseball/Master_Copy_W_Catchers.csv")
pitchFraming <- pitchFraming[pitchFraming$PitcherTeam == "GEO_BUL",]
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

pitchFraming$count <- ifelse(pitchFraming$goodCall == 1, 0, ifelse(pitchFraming$strikeCalledBall == 1, -1, 1))
view(pitchFraming)

strikesGained <- pitchFraming %>% group_by(pitchFraming$Catcher)
view(strikesGained)

subset <-pitchFraming[c(7,83)]

data <- subset%>% 
  group_by(Catcher) %>% 
  summarise_all(sum)

data$runsSaved <- data$count * 0.135

data

myPlot <- ggplot(data, aes(x = runsSaved, y = Catcher, fill = (runsSaved > 0))) +
  geom_col() +
  coord_flip()
myPlot

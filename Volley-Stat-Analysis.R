library(ggplot2)
library(tidyverse)
library(dplyr)

#loading the data and check for missing values
setwd(getwd())
matchstatsdata <- read.csv("DataSet/matchStats.csv",header = TRUE,  sep = ";",dec = ".")

# Check for columns with missing values
colSums(is.na(matchstatsdata))
#summary(matchstatsdata)

# Adding column Home.Team_won to indicate whether the home team won 
matchstatsdata$Home.Team_won <- ifelse(matchstatsdata$Home.Team == matchstatsdata$Winner, 1, 0)

home_team = table(matchstatsdata$Home.Team)
away_team = table(matchstatsdata$Away.Team)
winner = table(matchstatsdata$Winner)
losser = table(matchstatsdata$Loser)


plot(home_team)
barplot(away_team)
barplot(winner)
barplot(losser)

matchstatsdata$Set1_Home_Win <- ifelse(
  matchstatsdata$Set1.Home - matchstatsdata$Set1.Away >= 2,
  1,
  0
)
matchstatsdata$Set2_Home_Win <- ifelse(
  matchstatsdata$Set2.Home - matchstatsdata$Set1.Away >= 2,
  1,
  0
)
matchstatsdata$Set3_Home_Win <- ifelse(
  matchstatsdata$Set3.Home - matchstatsdata$Set1.Away >= 2,
  1,
  0
)
matchstatsdata$Set4_Home_Win <- ifelse(
  matchstatsdata$Set4.Home - matchstatsdata$Set1.Away >= 2,
  1,
  0
)
matchstatsdata$Set5_Home_Win <- ifelse(
  matchstatsdata$Set5.Home - matchstatsdata$Set1.Away >= 2,
  1,
  0
)


library(ggplot2)
library(tidyverse)
library(dplyr)

#loading the data and check for missing values
setwd(getwd())
matchstatsdata <- read.csv("matchStats.csv",header = TRUE,  sep = ";",dec = ".")

# Check for columns with missing values
colSums(is.na(matchstatsdata))
#summary(matchstatsdata)

# Adding column Home.Team_won to indicate whether the home team won 
matchstatsdata$Home.Team_won <- ifelse(matchstatsdata$Home.Team == matchstatsdata$Winner, 1, 0)

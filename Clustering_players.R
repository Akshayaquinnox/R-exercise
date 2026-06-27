library(ggplot2)
library(tidyverse)
library(dplyr)

#loading the data and check for missing values
setwd(getwd())
playerstatsdata <- read.csv("DataSet/playerStats.csv",header = TRUE,  dec = ".") %>%
  as_tibble()%>%
  mutate(
    TotalErrors = rowSums(select(., ends_with("Errors")))
  )%>%
  mutate(
    Height = as.numeric(str_remove(Height, "cm")))
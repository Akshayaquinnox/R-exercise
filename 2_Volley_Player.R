library(ggplot2)
library(tidyverse)
library(dplyr)

#loading the data and check for missing values
setwd(getwd())
playerstatsdata <- read.csv("playerStats.csv",header = TRUE,  dec = ".") %>%
  mutate(
    TotalErrors = rowSums(select(., ends_with("Errors")))
  )

pivotlonger(
  + cols = c(control, cond1, cond2),
  + names_to="condition",
  + values_to="measurement"
  + )

as_tibble(playerstatsdata)%>%
  mutate(
    Height = as.numeric(str_remove(Height, "cm"))
  )
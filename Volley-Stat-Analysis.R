library(ggplot2)
library(tidyverse)
library(dplyr)
library(GGally)

#loading the data and check for missing values
setwd(getwd())
matchstatsdata <- read.csv("DataSet/matchStats.csv",header = TRUE,  sep = ";",dec = ".")
colSums(is.na(matchstatsdata))


#Receptions.Home,Set4.Home,Set4.Away,Set5.Home,Set5.Away have NA values
#Set4 and Set5  Could be the case of MAR since the missing data occurs 
#for data entries where the winner is selected based on scores of previous three sets

#Receptions.Home could be the case of MCAR because the value does not depend on any variables


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
#outlier detection
ggplot(matchstatsdata,aes(x=Kills.Home,y=Home.Team))+geom_boxplot() 
ggplot(matchstatsdata,aes(x=Blocks.Home,y=Home.Team))+geom_boxplot()
ggplot(matchstatsdata,aes(x=Set1.Home,y=Home.Team))+geom_boxplot()
ggplot(matchstatsdata,aes(x=Set2.Home,y=Home.Team))+geom_boxplot()
ggplot(matchstatsdata,aes(x=Set3.Home,y=Home.Team))+geom_boxplot()
ggplot(matchstatsdata,aes(x=Set4.Home,y=Home.Team))+geom_boxplot()
ggplot(matchstatsdata,aes(x=Set5.Home,y=Home.Team))+geom_boxplot()

# sapply(colnames(matchstatsdata),function(x){
#   ggplot(matchstatsdata,aes(x=x,y=Home.Team))+geom_boxplot()
# })

#correlation
df_num=matchstatsdata[, sapply(matchstatsdata, is.numeric)]
cormatrix <- cor(df_num, use = "pairwise.complete.obs")
cormatrix
corrplot(cormatrix, method = "color")

#normality Detection for one variable Kills.Home
q_empirical <- sort(matchstatsdata$Kills.Home)
j_star <- (c(1:length(matchstatsdata$Kills.Home)
) - 0.5)/length(matchstatsdata$Kills.Home)
q_theoretical <- qnorm(j_star)
plot(q_theoretical, q_empirical, xlab="Theoretical Quantiles",ylab="Empirical Quantiles")
abline(lm(q_theoretical~q_empirical), col="red")

shapiro.test(q_empirical)

#applying normal distribution to all numeric columns
numeric_cols <- matchstatsdata[, sapply(matchstatsdata, is.numeric)]
par(mar = c(4, 3, 2, 2))
layout(matrix(1:8, ncol = 4, nrow = 2))
sapply(colnames(numeric_cols), function(x) {
  vec <- na.omit(numeric_cols[[x]])  # extract numeric vector and remove NAs
  
  qqnorm(vec, main = x, pch = 19, cex.lab = 1.5, cex.main = 1.5, ylab = "")
  qqline(vec, lwd = 2, col = "red")
})








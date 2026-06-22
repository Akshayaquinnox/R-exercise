install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(GGally)
library(MASS)
library(mice)

#1. loading the data and check for missing values

setwd(getwd())
matchstatsdata <- read.csv("DataSet/matchStats.csv",header = TRUE,  sep = ";",dec = ".")

#2 NA Values
colSums(is.na(matchstatsdata))   
#NA columns: Receptions.Home,Set4.Home,Set4.Away,Set5.Home,Set5.Away 

#MNAR:Set4,Set5----------Values Depend on previous 3 sets
#MCAR:Receptions.Home----Value does not depend on any variables

#3 IMPUTATION

#since the matches were not played in set4 and set5 replacing NA values to 0

sets_mnar <- c("Set4.Home","Set4.Away","Set5.Home","Set5.Away")

matchstatsdata[sets_mnar] <- lapply(matchstatsdata[sets_mnar],
                                    function(x) ifelse(is.na(x), 0, x))
colSums(is.na(matchstatsdata))


# Select only numeric variables for imputation model
num_data <- matchstatsdata[, sapply(matchstatsdata, is.numeric)]

# Run multiple imputation with PMM
imp <- mice(num_data, 
            m = 5,          # number of imputed datasets
            method = "pmm", # predictive mean matching
            seed = 123)
completed_data <- complete(imp, 1)   # choose imputed dataset 1
matchstatsdata$Receptions.Home <- completed_data$Receptions.Home

#4.
# Adding column Home.Team_won to indicate whether the home team won 
matchstatsdata$Home.Team_won <- ifelse(matchstatsdata$Home.Team == matchstatsdata$Winner, 1, 0)

#5.Outlier and Normality Detection
#outlier detection
ggplot(matchstatsdata,aes(x=Kills.Home,y=Home.Team))+geom_boxplot() 
num_cols <- names(matchstatsdata)[sapply(matchstatsdata, is.numeric)]

sapply(num_cols, function(col) {
  print(
    ggplot(matchstatsdata, aes(x = .data[[col]], y = Home.Team)) +
      geom_boxplot() +
      ggtitle(col) +
      theme_bw()
  )
})


#correlation
# df_num=matchstatsdata[, sapply(matchstatsdata, is.numeric)]
# cormatrix <- cor(df_num, use = "pairwise.complete.obs")
# cormatrix
# corrplot(cormatrix, method = "color")


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


#EDA

barplot(table(matchstatsdata$Winner))+title(main="Winner Stats")
barplot(table(matchstatsdata$Loser))+title(main="Losser Stats")

Home_winners <- as.data.frame(table(matchstatsdata$Home.Team, matchstatsdata$Winner))
colnames(Home_winners) <- c("HomeTeam", "Winner", "Count")


ggplot(Home_winners, aes(x = HomeTeam, y = Count, fill = Winner)) +
  geom_col(position = "stack") +
  labs(title = "Home Winner Stats",
       x = "Home Team",
       y = "Number of Wins",
       fill = "Winner") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Home_losers <- as.data.frame(table(matchstatsdata$Home.Team, matchstatsdata$Loser))
colnames(Home_losers) <- c("HomeTeam", "Loser", "Count")


ggplot(Home_losers, aes(x = HomeTeam, y = Count, fill = Loser)) +
  geom_col(position = "stack") +
  labs(title = "Home Lose Stats",
       x = "Home Team",
       y = "Number of Loses",
       fill = "Loser") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





ggplot(matchstatsdata, aes(x = Receptions.Away, y = Total.Points.Home)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship: Away Reception vs Home Total Points",
       x = "Receptions (Away)",
       y = "Total Points (Home)")

ggplot(matchstatsdata, aes(x = Receptions.Home, y = Total.Points.Home)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship: Away Reception vs Home Total Points",
       x = "Receptions (Home)",
       y = "Total Points (Home)")








#set3.Away 
#homecountry is Argentina has outlier 37 which can be genuine but was a long match
#homecountry is Poland has outlier has lowest 14 which can be genuine 
#homecountry is Turkey has outlier 22 and 29 which can be genuine
#homecountry is slovenia has outlier 22 and 29 which can be genuine

#Reception.Away
#lowest reception is serbia and they lost the game

#reception.Home
#Usa has highest 87 and they won against IRAN

#Digs.Away
# Serbia digs.away has highest of 106 digs still serbia lost slovania
#Serbia digs.away has lowest of 29 still serbia lost

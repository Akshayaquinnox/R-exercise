#loading the data and check for missing values
matchstatsdata <- read.csv("matchStats.csv",header = TRUE,  sep = ";",dec = ".")
colSums(is.na(matchstatsdata))
summary(matchstatsdata)

matchstatsdata$Home.Team_won <- ifelse(matchstatsdata$Home.Team == matchstatsdata$Winner, 1, 0)


#analyzing the kills.home
qqnorm(matchstatsdata$Kills.Home)
qqline(matchstatsdata$Kills.Home, col="red")
shapiro.test(matchstatsdata$Kills.Home) # results in W = 0.95812, p-value = 0.001134

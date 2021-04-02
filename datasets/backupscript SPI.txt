setwd("C:/Documents/thesis/datasets")
SPI <- read.csv("./Spi.csv")
library(lubridate)
library(tidyr)
library(stringr)
SPI


SPI$date <- as.Date(SPI$date)

SPI <- SPI %>% filter(date > "2018-07-01" & date < "2021-03-25")
SPI <- SPI %>% filter(league %in% c("Belgian Jupiler League", "Dutch Eredivisie", "French Ligue 1", "Turkish Turkcell Super Lig", "Barclays Premier League", 
                                    "Portuguese Liga", "Spanish Primera Division", "German Bundesliga", "Italy Serie A"))
colnames(SPI)[which(names(SPI) == "team1")] <- "HomeTeam"
colnames(SPI)[which(names(SPI) == "team2")] <- "AwayTeam"

#SPI <- SPI[2:23]
#SPI <- subset( SPI, select = -c(league_id, ))
#Data <- subset( Data, select = -a )



SPI <- SPI[!is.na(SPI$score1),]

write.csv(SPI,"C:/Documents/thesis/datasets/SPI2018_21.csv", row.names = FALSE)
write.csv(full_results, "C:/Documents/thesis/datasets/full_results.csv", row.names = FALSE)

Full_dataset_alan <- left_join(full_results, SPI, by = c("date", "HomeTeam"))

#
#x <- as.string(SPI$HomeTeam)
#y <- full_results$HomeTeam

#x<- unique(SPI$HomeTeam)
#y <- unique(full_results$HomeTeam)
#print(x)


#both <- x[x %in% y] # in both, same as call: intersect(first, second)
#onlyfirst <- x[!x %in% y] # only in 'first', same as: setdiff(first, second)
#onlysecond <- y[!y %in% x] # only in 'second', same as: setdiff(second, first)
#length(both)
#length(onlyfirst)
#length(onlysecond)
#print

#printList <- function(list) {
  
 # for (item in 1:length(list)) {
    
  #  print(head(list[[item]]))
    
#  }
#}

#printList(both)
library(tree)
printList(onlyfirst)
view(onlyfirst )

model <- lm(home_points ~ covid + occupancy, full_dataset)
summary(model)
model <- lm(home_points ~ covid + occupancy + covid*occupancy, full_dataset)
summary(model)
model <- lm(home_points ~ covid + occupancy + covid*occupancy + spi1 + spi2, full_dataset)
summary(model)
model <- lm(diff_points ~ covid + occupancy + covid*occupancy + spi1 + spi2 + avg_age + covid*avg_age + foreigners_used + covid*foreigners_used + covid*percentage_foreigners_used_away + percentage_foreigners_used_away + avg_age_away + covid*avg_age_away  + Importance_difference + importance1 + importance2 , full_dataset)
logmodel <- glm(home_win ~ covid + occupancy + covid*occupancy + spi1 + spi2 + avg_age + covid*avg_age + foreigners_used + covid*foreigners_used + covid*percentage_foreigners_used_away + percentage_foreigners_used_away + avg_age_away + covid*avg_age_away  + Importance_difference + importance1 + importance2 , full_dataset, family=binomial)
summary(model)
summary(logmodel)
tree <- tree(home_win ~ covid + occupancy  + spi1 + spi2 + avg_age  + foreigners_used  + percentage_foreigners_used_away + avg_age_away  + Importance_difference + importance1 + importance2 , full_dataset)
view(tree)
print(tree)
plot(tree)
summary(logmodel)
logmodel <- glm(home_win ~ covid + occupancy + covid*occupancy + spi1 + spi2 + avg_age + covid*avg_age + foreigners_used + covid*foreigners_used + covid*percentage_foreigners_used_away + percentage_foreigners_used_away + avg_age_away + covid*avg_age_away  + Importance_difference + importance1 + importance2 + League, full_dataset, family="poisson")
summary(logmodel)

summary(full_dataset$red_card_spread)
mean(full_dataset$red_card_spread)
sd(full_dataset$red_card_spread)
length(full_dataset$red_card_spread)
error <- qt(0.975,df=length(full_dataset$red_card_spread)-1)*sd(full_dataset$red_card_spread)/sqrt(length(full_dataset$red_card_spread))   
left <- mean(full_dataset$red_card_spread) - error
right <- mean(full_dataset$red_card_spread) + error

summary(full_dataset$yel_card_spread)
mean(full_dataset$yel_card_spread)
sd(full_dataset$yel_card_spread)
length(full_dataset$yel_card_spread)
error <- qt(0.975,df=length(full_dataset$yel_card_spread)-1)*sd(full_dataset$yel_card_spread)/sqrt(length(full_dataset$yel_card_spread))   
left <- mean(full_dataset$yel_card_spread) - error
right <- mean(full_dataset$yel_card_spread) + error

summary(full_dataset$fouls_spread)
mean(full_dataset$fouls_spread)
sd(full_dataset$fouls_spread)
length(full_dataset$fouls_spread)
error <- qt(0.975,df=length(full_dataset$fouls_spread)-1)*sd(full_dataset$fouls_spread)/sqrt(length(full_dataset$fouls_spread))   
left <- mean(full_dataset$fouls_spread) - error
right <- mean(full_dataset$fouls_spread) + error

install.packages("ggpubr")
library(ggpubr)
plot(ggplot(full_dataset))
ggqqplot(full_dataset$fouls_spread)
shapiro.test(full_dataset$fouls_spread)
ggqqplot(full_dataset$FTHG)
ggqqplot(full_dataset$FTAG)
ggqqplot(full_dataset$yel_card_spread)
ggqqplot(full_dataset$red_card_spread)
mean(full_dataset$home_win) 
mean(full_dataset$away_win)
mean(full_dataset$draw)
mean(full_dataset$home_points)
mean(full_dataset$away_points)
mean(season18_19$fouls_spread)
mean(season19_20$fouls_spread)
mean(season20_21$fouls_spread)
mean(season18_19$yel_card_spread)
mean(season19_20$yel_card_spread)
mean(season20_21$yel_card_spread)
mean(season18_19$red_card_spread)
mean(season19_20$red_card_spread)
mean(season20_21$red_card_spread)

covid_data <- full_dataset %>% filter(covid == 1)
non_covid_data <- full_dataset %>% filter(covid != 1)

mean(covid_data$home_win) 
mean(covid_data$away_win)
mean(covid_data$draw)
mean(covid_data$home_points)
mean(covid_data$away_points)
mean(covid_data$fouls_spread)
mean(covid_data$yel_card_spread)
mean(covid_data$red_card_spread)

mean(non_covid_data$home_win) 
mean(non_covid_data$away_win)
mean(non_covid_data$draw)
mean(non_covid_data$home_points)
mean(non_covid_data$away_points)
mean(non_covid_data$fouls_spread)
mean(non_covid_data$yel_card_spread)
mean(non_covid_data$red_card_spread)
mean(non_covid_data$foul_card_ratio_home)
mean(non_covid_data$foul_card_ratio_away)
mean(full_dataset$foul_card_ratio_away)
mean(covid_data$foul_card_ratio_away)
full_dataset.pca <- princomp(full_dataset)
str(full_dataset)


mean(full_dataset$foul_card_ratio_diff)

mean(full_dataset$foul_card_ratio_away)

install.packages("lavaan")
library(lavaan)
install.packages("semPlot")
library("semPlot")


set.seed(1234)


standard_full_dataset <- full_dataset %>% mutate_at(c("red_card_spread","fouls_spread","yel_card_spread" ,"foul_card_ratio_away","foul_card_ratio_home"), ~(scale(.) %>% as.vector))

model1sem <- '
       referee bias=~  fouls_spread +  red_card_spread + foul_card_ratio_diff + yel_card_spread 


'
fit1 <- sem(model1sem, data = standard_full_dataset)
summary(fit1, standardized = TRUE)

fitMeasures(fit1, c("cfi", "rmsea", "srmr"))
semPaths(fit1, what="paths", whatLabels = "stand", rotation = 1)

set.seed(6708)

model1 <- glm(home_win ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, full_dataset_alan_standardized, family = "binomial" )
model2 <- lm(goal_diff ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, full_dataset_alan_standardized)
summary(model1)
summary(model2)
model3 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, full_dataset_alan_standardized)
summary(model3)
model4 <- lm(home_win ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid + avg_attendance + avg_attendance * covid, full_dataset_alan_standardized)
summary(model4)             
model5 <- lm(xg_diff ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, full_dataset_alan_standardized )
model_simple <- lm(goal_diff ~ covid, data = full_dataset_alan)
summary(model_simple)
model_logit_simple <- glm(home_win ~ covid, data = full_dataset_alan)
summary(model_logit_simple)

summary(model1)
summary(model2)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
x<- logit2prob(coef(model1))
print(x)
exp(coefficients(model1))
summary(model5)
model6 <- glm(FTHG ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, full_dataset_alan_standardized, family = "poisson")
library(ggplot2)
library(sandwich)
library(msm)

summary(model6)
model7 <- glm(FTAG ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, full_dataset_alan_standardized, family = "poisson")

full_dataset_alan$stadium_size <- as.numeric(full_dataset_alan$stadium_size)
full_dataset_alan$avg_attendance <- as.numeric(full_dataset_alan$avg_attendance)
full_dataset_alan$covid <- as.factor(full_dataset_alan$covid)

full_dataset_alan_standardized <- full_dataset_alan %>% mutate_if(is.numeric, scale)
full_dataset_alan_standardized <- full_dataset_alan_standardized %>% mutate_if(is.integer, scale)

str(full_dataset_alan_standardized)
premier_league_dataset <- full_dataset_alan %>% filter(league == "Barclays Premier League")
Turkish_league_dataset <- full_dataset_alan %>% filter(league == "Turkish Turkcell Super Lig")


Portuguese_league_dataset <- full_dataset_alan %>% filter(league == "Portuguese Liga")
Spanish_league_dataset <- full_dataset_alan %>% filter(league == "Spanish Primera Division")
French_league_dataset <- full_dataset_alan %>% filter(league == "French Ligue 1")
Dutch_league_dataset <- full_dataset_alan %>% filter(league == "Dutch Eredivisie")
Belgian_league_dataset <- full_dataset_alan %>% filter(league == "Belgian Jupiler League")
Italian_league_dataset <- full_dataset_alan %>% filter(league == "Italy Serie A")

German_league_dataset <- full_dataset_alan %>% filter(league == "German Bundesliga")



model8 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, premier_league_dataset)
model9 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, Turkish_league_dataset)
model10 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, French_league_dataset)
model11 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, Belgian_league_dataset)
model12 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, German_league_dataset)
model13 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, Dutch_league_dataset)
model14 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, Spanish_league_dataset)
model15 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, Italian_league_dataset)
model16 <- lm(diff_point ~ covid  + occupancy + foreigners_spread + importance_diff + age_diff + rating_diff + occupancy * covid + age_diff * covid + foreigners_spread * covid, Portuguese_league_dataset)
model17 <- glm(away_win~ covid, data = full_dataset_alan, family = "binomial")
model18 <- glm(away_win ~ covid + occupancy + covid*occupancy, data = full_dataset_alan, family = "binomial")
model19 <- glm(away_win ~ covid + occupancy + covid*occupancy + age_diff + age_diff * covid, data = full_dataset_alan, family = "binomial")
model20 <- glm(away_win ~ covid + occupancy + covid*occupancy + age_diff + age_diff * covid + foreigners_spread + covid * foreigners_spread, data = full_dataset_alan, family = "binomial")
model21 <- glm(away_win ~ covid + occupancy + covid*occupancy + age_diff + age_diff * covid + foreigners_spread + covid * foreigners_spread + rating_diff, data = full_dataset_alan, family = "binomial")
model22 <- glm(away_win ~ covid + occupancy + covid*occupancy + age_diff + age_diff * covid + foreigners_spread + covid * foreigners_spread + rating_diff + importance_diff + as.factor(crowdsize), data = full_dataset_alan, family = "binomial")
model23 <- lm(FTHG ~ covid + occupancy + covid*occupancy + age_diff + age_diff * covid + foreigners_spread + covid * foreigners_spread + rating_diff + importance_diff + crowdsize, data = full_dataset_alan)
model24 <- lm(FTHG ~ covid + occupancy + covid*occupancy + age_diff + age_diff * covid + foreigners_spread + covid * foreigners_spread + rating_diff + importance_diff + crowdsize + crowdsize * covid, data = full_dataset_alan)

model25 <- lm(FTHG ~ covid + occupancy + covid*occupancy + age_diff + age_diff * covid + foreigners_spread + covid * foreigners_spread + rating_diff + importance_diff + crowd, data = full_dataset_alan)

summary(model25)


stargazer(model17,model18,model19,model20,model21,model22, 
          type = "html",
          title = "Model Output",    
          out="model_output3.html")
dataset <- full_dataset_alan[ , -which(names(full_dataset_alan) %in% c("Date", "HomeTeam", "AwayTeam", "league", "season", "away_win", "home_points", "away_points", "home_win", "draw", "FTHG", "FTAG", "foreigners_home", "avg_age_home", "foreigners_away", "avg_age_away", "HY", "AY", "HR", "AR", "HC", "AC", "HF", "AF", "spi1", "spi2", "importance1", "importance2", "xg1", "xg2", "stadium_size", "avg_attendance", "covid", "occupancy", "age_diff", "importance_diff", "rating_diff", "foreigners_spread"))]
dataset <- as.numeric(dataset)
dataset[] <- lapply(dataset, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
dataset <- dataset %>% mutate_if(is.integer, as.numeric)

sapply(dataset, class)
summary(dataset)
x<- round(cor(dataset), 2)
dataset <- dataset[!is.na(dataset$xg_diff),]
dataset <- dataset[!is.na(dataset$red_card_ratio_home), ]
dataset <- dataset[!is.na(dataset$shots_ratio_away), ]
dataset <- dataset[!is.na(dataset$shots_ratio_home), ]
str(dataset)
dataset_team_performance <- dataset[, which(names(dataset) %in%  c("HS", "AS", "HST", "AST", "xg_diff", "shots_ratio_home", "shots_ratio_away", "goal_diff", "diff_point"))]
dataset_referee_bias <- dataset[, which(names(dataset) %in%  c("yel_card_spread", "red_card_spread", "yel_card_ratio_home", "yel_card_ratio_away", "red_card_ratio_home", "red_card_ratio_away", "foul_spread"))]
m1a  <- ' f1  =~  HS+ AS + HST +  AST +  xg_diff +  shots_ratio_home +  shots_ratio_away +  goal_diff +  diff_point
          f2 =~ yel_card_spread + red_card_spread + foul_spread 
          f1 ~~ 0*f2'
#+ yel_card_ratio_home + yel_card_ratio_away + red_card_ratio_home + red_card_ratio_away +
twofac <- cfa(m1a, data = dataset,std.lv=TRUE)
dataset <- dataset %>% mutate_if(is.integer, scale)
dataset <- dataset %>% mutate_if(is.numeric, scale)
summary(twofac, fit.measures=TRUE,standardized=TRUE)
#library(tidyverse)
#library(caret)
#library(leaps)
#library(MASS)
full.model <- lm(shots_ratio_hom ~. , data = dataset)
##stepwise regression
step.model <- stepAIC(model2, direction = "forward", 
                     trace = FALSE)
#summary(step.model)
#model_shots <- lm(shots_ratio_home ~ covid, data = full_dataset_alan)
#summary(model_shots)

#install.packages("mlogit")
library(mlogit)
library(nnet)
#install.packages("MNP")
library(MNP)
library(foreign)

table(full_dataset_alan$Result)
?multinom
multil <- mnp(Result ~  covid + occupancy + covid*occupancy + age_diff + age_diff * covid + foreigners_spread + covid * foreigners_spread + rating_diff + importance_diff + as.factor(crowdsize), data = full_dataset_alan)
summary(multil)

logmul <- multinom(Result ~ covid +
                     occupancy + covid*occupancy + age_diff + age_diff * covid + foreigners_spread + covid * foreigners_spread + rating_diff + importance_diff, data = full_dataset_alan)
summary(logmul)

z <- summary(logmul)$coefficients/summary(logmul)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
 
library(asht)
library(pander)
library(compare)
library(WMWssp)
library(lavaan)
library(ggpubr)
library(stringr)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(qwraps2)
library(gridExtra)
library(stargazer)
library(psych)
library(readr)
library(sjPlot)
library(caret)
library(Hmisc)
library(nortest)
library(semPlot)
library(knitr)
require(gvlma)
library(haven)
library(trafo)
library(plm)
setwd("C:/Documents/thesis/datasets")
SPI <- read.csv("./Spi.csv")
SPI$date <- as.Date(SPI$date)
SPI <- SPI %>% filter(date > "2018-07-01" & date < "2021-03-25")
SPI <- SPI %>% filter(league %in% c("Belgian Jupiler League", "Dutch Eredivisie", "French Ligue 1", "Turkish Turkcell Super Lig", "Barclays Premier League", 
                                    "Portuguese Liga", "Spanish Primera Division", "German Bundesliga", "Italy Serie A"))
colnames(SPI)[which(names(SPI) == "team1")] <- "HomeTeam"
colnames(SPI)[which(names(SPI) == "team2")] <- "AwayTeam"
SPI <- SPI[!is.na(SPI$score1),]
SPI <- SPI %>% 
  mutate(HomeTeam = str_replace_all(HomeTeam, "Sporting CP", "Sp Lisbon")) %>%  
  mutate(HomeTeam = str_replace_all(HomeTeam, "Sporting de Charleroi", "Charleroi" )) %>% 
  mutate(HomeTeam = str_replace_all(HomeTeam, "AZ", "AZ Alkmaar" )) %>%
  mutate(HomeTeam = str_replace_all(HomeTeam, "PSV EIndhoven", "PSV Eindhoven" )) %>%
  mutate(HomeTeam = str_replace_all(HomeTeam, "RKC", "Waalwijk" ) )%>%  
  mutate(HomeTeam = str_replace_all(HomeTeam, "NAC", "NAC Breda" )) %>%
  mutate(HomeTeam = str_replace_all(HomeTeam, "Sparta", "Sparta Rotterdam")) %>%
  mutate(HomeTeam = str_replace_all(HomeTeam, "Girona FC", "Girona"))
results18_19 <- read.csv("./2018/Results2018-19.csv")
results19_20 <- read.csv("./2019/Results2019-20.csv")
results20_21 <- read.csv("./2020/Results2020-21.csv")
moderator18_19 <- read.csv("./2018/transfermarkt3good.csv")
moderator19_20 <- read.csv("./2019/transfermarkt2good.csv")
moderator20_21 <- read.csv("./2020/transfermarktgood.csv")
moderator18_19 <-  moderator18_19[ , -which(names(moderator18_19) %in% c("season"))]
moderator19_20 <-  moderator19_20[ , -which(names(moderator19_20) %in% c("season"))]
moderator20_21 <-  moderator20_21[ , -which(names(moderator20_21) %in% c("season"))]
colnames(moderator18_19) <- c( "club", "avg_age_home", "players_used_home", "foreigners_home", "stadium_size", "avg_attendance", "occupancy", "covid", "covid_occupancy")
colnames(moderator19_20) <- c( "club", "avg_age_home", "players_used_home", "foreigners_home", "stadium_size", "avg_attendance", "occupancy", "covid", "covid_occupancy")
colnames(moderator20_21) <- c( "club", "avg_age_home", "players_used_home", "foreigners_home", "stadium_size", "avg_attendance", "occupancy", "covid", "covid_occupancy")
results19_20 <-  results19_20[ , -which(names(results19_20) %in% c("Time"))]
results20_21 <- results20_21[ , -which(names(results20_21) %in% c("Time"))]
results18_19 <- results18_19[1:22]
results19_20 <- results19_20[1:22]
results20_21 <- results20_21[1:22]
results18_19$club <- results18_19$HomeTeam 
results19_20$club <- results19_20$HomeTeam 
results20_21$club <- results20_21$HomeTeam 
results18_19$club2 <- results18_19$AwayTeam
results19_20$club2 <- results19_20$AwayTeam
results20_21$club2 <- results20_21$AwayTeam
moderator18_19$club2 <- moderator18_19$club
moderator19_20$club2 <- moderator19_20$club
moderator20_21$club2 <- moderator20_21$club
merge18_19 <- left_join(results18_19, moderator18_19, by = "club")
merge19_20 <- left_join(results19_20, moderator19_20, by = "club")
merge20_21 <- left_join(results20_21, moderator20_21, by = "club")
merge18_19$club2 <- merge18_19$AwayTeam
merge19_20$club2 <- merge19_20$AwayTeam
merge20_21$club2 <- merge20_21$AwayTeam
merge18_19 <- left_join(merge18_19, moderator18_19, by = "club2")
merge19_20 <- left_join(merge19_20, moderator19_20, by = "club2")
merge20_21 <- left_join(merge20_21, moderator20_21, by = "club2")
df_results <- rbind(merge18_19, merge19_20, merge20_21)
SPI$Date <- SPI$date
SPI$Date <- as.Date(SPI$Date)
df_results$Date <- as.Date(df_results$Date, format = "%d/%m/%Y")
Full_dataset_alan <- left_join(df_results, SPI, by = c("Date", "HomeTeam"))
Full_dataset_alan <- Full_dataset_alan[ , -which(names(Full_dataset_alan) %in% c("Div", "HTR", "HTHG", "HTAG", "club.x","club2.x", "season.y", "covid.x","covid.y" ,"club.y","club2","AwayTeam.y" ,"club2.y", "club.y","covid_occupancy.y", "covid_occupancy.x", "date", "date.y", "league_id", "AwayTeam.y", "score1", "score2", "stadium_size.y", "occupancy.y", "avg_attendance.y"))]
names(Full_dataset_alan)[names(Full_dataset_alan) == 'avg_age_home.x'] <- 'avg_age_home'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'avg_age_home.y'] <- 'avg_age_away'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'players_used_home.x'] <- 'players_used_home'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'players_used_home.y'] <- 'players_used_away'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'AwayTeam.x'] <- 'AwayTeam'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'foreigners_home.x'] <- 'foreigners_home'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'foreigners_home.y'] <- 'foreigners_away'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'avg_attendance.x'] <- 'avg_attendance'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'stadium_size.x'] <- 'stadium_size'
names(Full_dataset_alan)[names(Full_dataset_alan) == 'occupancy.x'] <- 'occupancy'
full_dataset_alan <- Full_dataset_alan %>% mutate(yel_card_spread = AY - HY, rating_diff = spi1-spi2, xg_diff = xg1 - xg2, age_diff = avg_age_home - avg_age_away, red_card_spread = AR - HR, importance_diff = importance1 - importance2  )
full_dataset_alan <- full_dataset_alan %>% mutate(covid = ifelse(Date > "2020-04-01", 1, 0))
full_dataset_alan <- full_dataset_alan %>% mutate(home_win = ifelse(FTHG > FTAG, 1, 0))
full_dataset_alan <- full_dataset_alan %>% mutate(away_win = ifelse(FTHG < FTAG, 1, 0))
full_dataset_alan <- full_dataset_alan %>% mutate(draw = ifelse(FTHG == FTAG, 1, 0))
full_dataset_alan <- full_dataset_alan %>% mutate(home_points = ifelse(FTHG > FTAG, 3, ifelse(FTHG < FTAG, 0, 1)))
full_dataset_alan <- full_dataset_alan %>% mutate(away_points = ifelse(FTHG < FTAG, 3, ifelse(FTHG > FTAG, 0, 1)))
full_dataset_alan <- full_dataset_alan %>% mutate(yel_card_ratio_home = HY/HF)
full_dataset_alan <- full_dataset_alan %>% mutate(red_card_ratio_home = HY/HF)
full_dataset_alan <- full_dataset_alan %>% mutate(yel_card_ratio_away = AY/AF)
full_dataset_alan <- full_dataset_alan %>% mutate(red_card_ratio_away = AR/AF)
full_dataset_alan <- full_dataset_alan %>% mutate(shots_ratio_home = HST/HS)
full_dataset_alan <- full_dataset_alan %>% mutate(shots_ratio_away = AST/AS)
full_dataset_alan <- full_dataset_alan %>% mutate(goal_diff = FTHG - FTAG)
full_dataset_alan <- full_dataset_alan %>% mutate(diff_point = home_points - away_points)
full_dataset_alan <- full_dataset_alan %>% mutate(foul_spread = AF - HF)
full_dataset_alan <- full_dataset_alan %>% mutate(foreigners_spread = foreigners_home - foreigners_away)
full_dataset_alan <- full_dataset_alan %>% mutate(percentage_points_home = home_points / (home_points + away_points))
full_dataset_alan <- full_dataset_alan %>% mutate(percentage_points_away = away_points / (away_points + home_points))
full_dataset_alan <- full_dataset_alan %>% mutate(Result = ifelse(FTHG > FTAG, "homewin", ifelse(FTHG < FTAG, "awaywin","draw")))
full_dataset_alan <-  full_dataset_alan[ , -which(names(full_dataset_alan) %in% c("FTR", "players_used_home", "players_used_away", "prob1", "prob2", "probtie", "proj_score1", "proj_score2", "nsxg1", "nsxg2", "adj_score1", "adj_score2", "home_points_percentage_total", "away_points_percentage_total"))]
full_dataset_alan <- full_dataset_alan %>% mutate(crowdsize = ifelse(avg_attendance < 20000, "small", ifelse(avg_attendance > 20000 & avg_attendance < 40000, "medium", "large")))
full_dataset_alan <- full_dataset_alan %>% mutate(crowd = ifelse(avg_attendance < 10000, "extrasmall", ifelse(avg_attendance > 10000 & avg_attendance < 20000, "small", ifelse(avg_attendance > 30000 & avg_attendance < 40000,"medium", ifelse(avg_attendance > 40000 & avg_attendance < 50000, "big", "large")))))
full_dataset_alan <- full_dataset_alan %>% mutate(var = ifelse(season == 2018 & (league == "Barclays Premier League" | league == "Portuguese Liga"),0, 1))
full_dataset_alan <- full_dataset_alan %>% mutate(CornerDifference = HC - AC)
full_dataset_alan <- full_dataset_alan %>% mutate(ShotsDifference = HS - AC)
full_dataset_alan <- full_dataset_alan %>% mutate(ShotsTargetDifference = HST - AST)
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='yel_card_spread')] <- 'YellowCardDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='red_card_spread')] <- 'RedCardDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='foul_spread')] <- 'FoulDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='age_diff')] <- 'AgeDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='xg_diff')] <- 'ExpectedGoalsDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='importance_diff')] <- 'ImportanceDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='rating_diff')] <- 'RatingDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='var')] <- 'VAR'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='occupancy')] <- 'OccupancyRate'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='goal_diff')] <- 'GoalDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='percentage_points_home')] <- 'PercentagePointsHome'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='foreigners_spread')] <- 'ForeignersShareDifference'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='crowdsize')] <- 'Crowdsize'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='avg_attendance')] <- 'AverageAttendance'
colnames(full_dataset_alan)[which(colnames(full_dataset_alan)=='diff_point')] <- 'PointsDifference'
full_dataset_alan$AverageAttendance <- as.numeric(full_dataset_alan$AverageAttendance)
as.numeric(gsub(",","",full_dataset_alan$AverageAttendance,fixed=TRUE))



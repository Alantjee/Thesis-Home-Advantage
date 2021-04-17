library(asht)
library(pander)
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
full_dataset_alan <- Full_dataset_alan %>% mutate(yel_card_spread = HY - AY, rating_diff = spi1-spi2, xg_diff = xg1 - xg2, age_diff = avg_age_home - avg_age_away, red_card_spread = HR - AR, importance_diff = importance1 - importance2  )
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
full_dataset_alan <- full_dataset_alan %>% mutate(foul_spread = HF - AF)
full_dataset_alan <- full_dataset_alan %>% mutate(foreigners_spread = foreigners_home - foreigners_away)
full_dataset_alan <- full_dataset_alan %>% mutate(percentage_points_home = home_points / (home_points + away_points))
full_dataset_alan <- full_dataset_alan %>% mutate(percentage_points_away = away_points / (away_points + home_points))
full_dataset_alan <- full_dataset_alan %>% mutate(Result = ifelse(FTHG > FTAG, "homewin", ifelse(FTHG < FTAG, "awaywin","draw")))
full_dataset_alan <-  full_dataset_alan[ , -which(names(full_dataset_alan) %in% c("FTR", "players_used_home", "players_used_away", "prob1", "prob2", "probtie", "proj_score1", "proj_score2", "nsxg1", "nsxg2", "adj_score1", "adj_score2", "home_points_percentage_total", "away_points_percentage_total"))]
full_dataset_alan <- full_dataset_alan %>% mutate(crowdsize = ifelse(avg_attendance < 20000, "small", ifelse(avg_attendance > 20000 & avg_attendance < 400000, "medium", "large")))
full_dataset_alan <- full_dataset_alan %>% mutate(crowd = ifelse(avg_attendance < 10000, "extrasmall", ifelse(avg_attendance > 10000 & avg_attendance < 20000, "small", ifelse(avg_attendance > 30000 & avg_attendance < 40000,"medium", ifelse(avg_attendance > 40000 & avg_attendance < 50000, "big", "large")))))
full_dataset_alan <- full_dataset_alan %>% mutate(var = ifelse(season == 2018 & (league == "Barclays Premier League" | league == "Portuguese Liga"),0, 1))

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

full_dataset_alan$AverageAttendance <- as.numeric(full_dataset_alan$AverageAttendance)
as.numeric(gsub(",","",full_dataset_alan$AverageAttendance,fixed=TRUE))


covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)



mydata <- data.frame(result=as.factor(full_dataset_alan$Result),
                     pre_post_covid = as.factor(full_dataset_alan$covid))

mytab <- table(mydata)
res <- chisq.test(mytab)

stargazer(res,
          type = "html",
          out = "chisquare.html")
# look at the table:
mytab <- with(mydata,table(result,pre_post_covid)) 

res$p.value
res$estimate
 table(covid_data$away_win, covid_data$home_win)
 table(non_covid_data$away_win, non_covid_data$home_win)
 
 hist(foreigners_spread)
 hist(full_dataset_alan$foreigners_spread)
 hist(full_dataset_alan$age_diff)
 hist(full_dataset_alan$occupancy)
 hist(full_dataset_alan$importance_diff)
 hist(full_dataset_alan$xg_diff)
 hist(full_dataset_alan$goal_diff)
 hist(full_dataset_alan$home_win)

chisq.test(non_covid_data$Result, non_covid_data$away_win)
 prop.test(x = c(1210, 1014), n = c(2334, 1539), alternative = "less")
summary(res)
prop.test(x = C(), n, p = NULL, alternative = "two.sided",
          correct = TRUE)

win_home_covid <- mean(covid_data$home_win)
win_away_covid <- mean(covid_data$away_win)
win_home_noncovid <- mean(non_covid_data$home_win)
win_away_noncovid <- mean(non_covid_data$away_win)
sd_home_covid <- sd(covid_data$home_win)
sd_away_covid <- sd(covid_data$away_win)
sd_home_noncovid <- sd(non_covid_data$home_win)
sd_away_noncovid <- sd(non_covid_data$away_win)
se_home_covid <- sd_home_covid/sqrt(length(covid_data))
se_away_covid <- sd_away_covid/sqrt(length(covid_data))
se_home_noncovid <- sd_home_noncovid/sqrt(length(non_covid_data))
se_away_noncovid <- sd_away_noncovid/sqrt(length(non_covid_data))
situation <- factor(c("Home win covid","Away win covid","Home win pre covid", "Away win pre covid"))
meanwin <- c(win_home_covid,win_away_covid, win_home_noncovid, win_away_noncovid)
se <- c(se_home_covid, se_away_covid, se_home_noncovid, se_away_noncovid)
df_mean_win <- cbind(situation, meanwin, se)
df_mean_win <- data.frame(df_mean_win)
str(df_mean_win)
df_mean_win$situation <- factor(situation, levels = c("Home win pre covid","Away win pre covid","Home win covid", "Away win covid"))
levels(df_mean_win$situation)
df_mean_win
plot_mean_win <- ggplot(df_mean_win, aes(x = situation, y = meanwin, 
                                           ymin = meanwin-se, ymax = meanwin+se)) +
                        geom_bar(aes(color = situation), stat = "identity", fill ="white") + 
                        geom_errorbar(aes(color = situation), width = 0.2) + 
  xlab("Home vs Away wins") +
  ylab("Average wins") +
  ggtitle("Wins home and away pre and post covid") +
  theme_minimal()
plot_mean_win





x1 <- wilcox.test(covid_data$FTHG, non_covid_data$FTHG)
x1
x2 <- wilcox.test(covid_data$FTAG, non_covid_data$FTAG)
x2
x3 <-wilcox.test(covid_data$HY, non_covid_data$HY) 
x3
x4 <-wilcox.test(covid_data$AY, non_covid_data$AY)
x4
x5 <-wilcox.test(covid_data$HR, non_covid_data$HR)
x5
x6 <-wilcox.test(covid_data$AR, non_covid_data$AR)
x6
x7 <-wilcox.test(covid_data$HF, non_covid_data$HF)
x7
x8 <-wilcox.test(covid_data$AF, non_covid_data$AF)
x8
x9 <-chisq.test(covid_data$away_win, non_covid_data$away_win)
x9
x10 <-chisq.test(covid_data$home_win, non_covid_data$home_win)
x10
x11 <-wilcox.test(covid_data$HS, non_covid_data$HS)
x11
x12 <-wilcox.test(covid_data$AS, non_covid_data$AS)
x12
x13 <-wilcox.test(covid_data$xg1, non_covid_data$xg1)
x13
x14 <-wilcox.test(covid_data$xg2, non_covid_data$xg2)
x14
x15 <- wilcox.test(covid_data$home_points, non_covid_data$home_points)
x15
x16 <- wilcox.test(covid_data$away_points, non_covid_data$away_points)
x16
x17 <- wilcox.test(covid_data$PercentagePointsHome, non_covid_data$PercentagePointsHome)
x17
x18 <- wilcox.test(covid_data$percentage_points_away, non_covid_data$percentage_points_away)
x18
x19 <- wilcox.test(covid_data$HST, non_covid_data$HST)
x19
x20 <- wilcox.test(covid_data$AST, non_covid_data$AST)
x20
x21 <- wilcox.test(covid_data$diff_point, non_covid_data$diff_point)
x21
x22 <- wilcox.test(covid_data$YellowCardDifference, non_covid_data$YellowCardDifference)
x22
x23 <- wilcox.test(covid_data$RedCardDifference, non_covid_data$RedCardDifference)
x23
x24 <- wilcox.test(covid_data$FoulDifference, non_covid_data$FoulDifference)
x24
x25 <- wilcox.test(covid_data$GoalDifference, non_covid_data$GoalDifference)
x25

a<- mean(covid_data$YellowCardDifference)
b<- mean(non_covid_data$YellowCardDifference)
c<- mean(non_covid_data$RedCardDifference)
d<- mean(covid_data$RedCardDifference)
e<- mean(non_covid_data$FoulDifference)
f<- mean(covid_data$FoulDifference)
g <- mean(non_covid_data$percentage_points_away)
h <- mean(covid_data$percentage_points_away)
i <- mean(non_covid_data$GoalDifference)
j <- mean(covid_data$GoalDifference)

k <- mean(non_covid_data$ExpectedGoalsDifference)
L <- mean(covid_data$ExpectedGoalsDifference)
M <- wmwTest(covid_data$YellowCardDifference, non_covid_data$YellowCardDifference, alternative = c("two.sided", "less", "greater"))
N <- wmwTest(covid_data$RedCardDifference, non_covid_data$RedCardDifference, alternative = c("two.sided", "less", "greater"))
O <- wmwTest(covid_data$ExpectedGoalsDifference, non_covid_data$ExpectedGoalsDifference, alternative = c("two.sided", "less", "greater"))
P <- wmwTest(covid_data$FoulDifference, non_covid_data$FoulDifference, alternative = c("two.sided", "less", "greater"))
Q <- wmwTest(covid_data$GoalDifference, non_covid_data$GoalDifference, alternative = c("two.sided", "less", "greater"))
R <- wmwTest(covid_data$PercentagePointsHome, non_covid_data$PercentagePointsHome, alternative = c("two.sided", "less", "greater"))
S <- wmwTest(covid_data$HY, non_covid_data$HY, alternative = c("two.sided", "less", "greater"))
t <- wmwTest(covid_data$AY, non_covid_data$AY, alternative = c("two.sided", "less", "greater"))
U <- wmwTest(covid_data$HR, non_covid_data$HR, alternative = c("two.sided", "less", "greater"))
V <- wmwTest(covid_data$AR, non_covid_data$AR, alternative = c("two.sided", "less", "greater"))
W <- wmwTest(covid_data$AF, non_covid_data$AF, alternative = c("two.sided", "less", "greater"))
X <- wmwTest(covid_data$HF, non_covid_data$HF, alternative = c("two.sided", "less", "greater"))
Y <- wmwTest(covid_data$FTHG, non_covid_data$FTHG, alternative = c("two.sided", "less", "greater"))
Z <- wmwTest(covid_data$FTAG, non_covid_data$FTAG, alternative = c("two.sided", "less", "greater"))
A <- wmwTest(covid_data$home_points, non_covid_data$home_points, alternative = c("two.sided", "less", "greater"))
B <- wmwTest(covid_data$away_points, non_covid_data$away_points, alternative = c("two.sided", "less", "greater"))
C <- wmwTest(covid_data$HS, non_covid_data$HS, alternative = c("two.sided", "less", "greater"))
D <- wmwTest(covid_data$AS, non_covid_data$AS, alternative = c("two.sided", "less", "greater"))
E <- wmwTest(covid_data$HST, non_covid_data$HST, alternative = c("two.sided", "less", "greater"))
G <- wmwTest(covid_data$AST, non_covid_data$AST, alternative = c("two.sided", "less", "greater"))
H <- wmwTest(covid_data$xg1, non_covid_data$xg1, alternative = c("two.sided", "less", "greater"))
I <- wmwTest(covid_data$xg2, non_covid_data$xg2, alternative = c("two.sided", "less", "greater"))
J <- wmwTest(covid_data$percentage_points_away, non_covid_data$percentage_points_away, alternative = c("two.sided", "less", "greater"))
K <- chisq.test(covid_data$home_win, non_covid_data$home_win)

table(count(non_covid_data$Result, covid_data$Result))
non_covid_data$Result <- as.factor(non_covid_data$Result)
count(non_covid_data$Result)
#percentage home win and percentage away win

x<- pander(R)
prop.test(x = c(1014, 1210), n = c(1539, 2334),
          alternative = "two.sided")


hist(covid_data$HST)


ggqqplot(covid_data$ExpectedGoalsDifference)
hist(covid_data$ExpectedGoalsDifference)
shapiro.test(covid_data$HST)
shapiro.test(non_covid_data$HST)
shapiro.test(covid_data$AST)
shapiro.test(covid_data$FoulDifference)
shapiro.test(covid_data$ExpectedGoalsDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(xg1))
non_covid_data <- non_covid_data %>% filter(!is.na(xg2))
covid_data <- covid_data %>% filter(!is.na(xg1))
covid_data <- covid_data %>% filter(!is.na(xg2))
mean(non_covid_data$xg1)
mean(covid_data$xg1)
mean(non_covid_data$xg2)
mean(covid_data$xg2)


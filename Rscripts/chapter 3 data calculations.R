covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)

hist(foreigners_spread)
hist(full_dataset_alan$foreigners_spread)
hist(full_dataset_alan$age_diff)
hist(full_dataset_alan$occupancy)
hist(full_dataset_alan$importance_diff)
hist(full_dataset_alan$xg_diff)
hist(full_dataset_alan$goal_diff)
hist(full_dataset_alan$home_win)
model_variables %>% 
  pairs.panels()




shapiro.test(covid_data$ExpectedGoalsDifference)

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
J <- wmwTest(covid_data$percentage_points_away, non_covid_data$percentage_points_away, alternative = c("two.sided", "less", "greater"))

H <- wmwTest(covid_data$xg1, non_covid_data$xg1, alternative = c("two.sided", "less", "greater"))
I <- wmwTest(covid_data$xg2, non_covid_data$xg2, alternative = c("two.sided", "less", "greater"))
l <- t.test(covid_data$xg1, non_covid_data$HG, paired = FALSE)
x<- pander(l)
prop.test(x = c(1014, 1210), n = c(1539, 2334),
          alternative = "two.sided")


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
ks.test(full_dataset_alan$YellowCardDifference, y='pnorm',alternative='two.sided')


normality <- function(x){
  ad.test(x)
}
sapply(model_variables, normality)


root<- function(x){
  sqrt(x)
}
sapply(model_variables, root)

summary(model_variables)

#data check

model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("ImportanceDifference","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","ExpectedGoalsDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]
str(model_variables)


set.seed(123)
model_variables$spl=sample.split(model_variables$covid,SplitRatio=0.7)
train=subset(model_variables, full_dataset_alan$spl==TRUE)
test=subset(model_variables, full_dataset_alan$spl==FALSE)
summary(train)
train <- train %>% filter(!is.na(ImportanceDifference))
test <- test %>% filter(!is.na(ImportanceDifference))
ggplot(train, aes(AgeDifference, GoalDifference ) )+
  geom_point() +
  stat_smooth()

training.samples <- model_variables %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- model_variables[training.samples, ]
test.data <- model_variables[-training.samples, ]
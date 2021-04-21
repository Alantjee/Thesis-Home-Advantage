#our_summary1 <-
#  list("Goals home" =
#        list("min"       = ~ min(FTHG),
#            "max"       = ~ max(FTHG),
#           "mean (sd)" = ~ qwraps2::mean_sd(FTHG)),
#   "Goals away" =
#    list("min"       = ~ min(FTAG),
#        "median"    = ~ median(FTAG),
#       "max"       = ~ max(FTAG),
#      "mean (sd)" = ~ qwraps2::mean_sd(FTAG)),
# "Points home" =
#   list("min"       = ~ min(home_points),
#        "max"       = ~ max(home_points),
#        "mean (sd)" = ~ qwraps2::mean_sd(home_points)),
# "Points away" =
#   list("min" = ~ min(away_points),
#        "max"  = ~ max(away_points),
#        "mean (sd)"  = ~ qwraps2::mean_sd(away_points)))
#season18_19 <- full_dataset_alan %>% filter(season == 2018)
#season19_20 <- full_dataset_alan %>% filter(season == 2019)
#season20_21 <- full_dataset_alan %>% filter(season == 2020)
#ghost_games <- full_dataset_alan %>% filter(covid == 1)
#non_ghost_games <- full_dataset_alan %>% filter(covid !=1)


#mci <- mean_ci(season18_19$diff_point)
#str(mci)

##  'qwraps2_mean_ci' Named num [1:3] 20.1 18 22.2
##  - attr(*, "names")= chr [1:3] "mean" "lcl" "ucl"
##  - attr(*, "alpha")= num 0.05
#mci
## [1] "20.09 (18.00, 22.18)"
#print(mci, show_level = TRUE)
## [1] "20.09 (95% CI: 18.00, 22.18)"  


# difference in means
#mpvals <-
# sapply(
#    list(lm(home_points ~ season,  data = full_dataset_alan),
#        lm(diff_point ~ season, data = full_dataset_alan.),
#       lm(yel_card_spread ~ season,   data = full_dataset_alan)),
#  extract_fpvalue)

#whole <- summary_table(full_dataset_alan, our_summary1)
#full_dataset_alan$season <- as.factor(full_dataset_alan$season)
#whole_by_season <- summary_table(dplyr::group_by(full_dataset_alan, season), our_summary1 )

#mpvals <-
# sapply(
#  list(lm(home_points ~ season,  data =full_dataset_alan),
#       lm(diff_point ~ season, data =full_dataset_alan.),
#      lm(yel_card_spread ~ season,   data =full_dataset_alan)),
# extract_fpvalue)
#summary_table(full_dataset_alan)

#plot(whole_by_season)

#png("test.png", height=1000, width=200)
#p<-tableGrob(whole_by_season)
#grid.arrange(p)
#dev.off()
#mean()
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

setwd("C:/Documents/thesis/datasets")

moderator_data_18 <- read.csv("")



stargazer(descriptivevariables,
          type = "text",
          title = "Descriptive Statistics", 
          column.labels = c("$X$", "$Y$"), 
          out="stats_descriptives.docx")

png("summarystatistics.png", height=2000, width=3000)
p<-tableGrob(t2)
grid.arrange(p)
dev.off()


#colnames(descriptivevariables) <- c("Home_goals")
most_important_stats <- cbind(important_stats[, 1:2], important_stats[, 8:9], important_stats[, 12:24], important_stats[, 34:35], important_stats[, 37:41], important_stats[, 46:47])
utmost_important_stats <- cbind(most_important_stats [,1:8], most_important_stats[, 11:14], most_important_stats[, 17:18], most_important_stats[, 20:26])
utmost_important_stats <- utmost_important_stats[ , -which(names(utmost_important_stats) %in% c("HF", "AF", "HY", "AY", "AR", "HR", "foreigners_home", "foreigners_away", "stadium_size" , "avg_attendance"))] 
#utmost_important_stats <- cbind(utmost_important_stats, full_dataset_alan$spi1 "spi_home", full_dataset_alan$spi2 = "spi_away", full_dataset_alan$prob1 = "prob_home_win", full_dataset_alan$prob2 = "prob_away_win", full_dataset_alan$probtie = "prob_tie", full_dataset_alan$foul_spread = "foul_spread") 
x <- describe(most_important_stats)
descrip<- stargazer(most_important_stats)
descrip <- as.data.frame(descrip)
summary(most_important_stats)
utmost_important_stats <-utmost_important_stats %>% 
  mutate_each(funs(if(is.integer(.)) as.numeric(.) else .))
columns <- colnames(utmost_important_stats)
t1 <- stargazer(
  utmost_important_stats[,columns], type = "text",digits = 2, decimals = 2,
  summary.stat = c("N", "min", "median","max", "median", "sd"))
t2 <- stargazer(most_important_stats, decimals = 2, digits = 2 , type = "text", ci=TRUE, ci.level = 0.95)
t2 <- table(t2)
print(t1)
library(here)
pdf(here("thesis", "datasets","descriptive table"))
(x)Q
dev.off()




contingency <- as.table(as.matrix(meanwin))
chisq.test(contingency)
res <- prop.test(x = c(490, 400), n = c(500, 500))
contingency


win_home_covid <- mean(covid_data$home_win) 
win_home_noncovid <- mean(non_covid_data$home_win)
win_away_covid <- mean(covid_data$away_win)
win_away_noncovid <- mean(non_covid_data$away_win)


count_home_win <- count(covid_data$home_win)
prop.test(x = c(1210, 1014), n = c(2334, 1539),
          alternative = "less")


ts = replicate(1000,t.test(covid_data$FTAG,non_covid_data$FTAG)$statistic)
range(ts)


table(count(non_covid_data$Result, covid_data$Result))
non_covid_data$Result <- as.factor(non_covid_data$Result)
count(non_covid_data$Result)
#percentage home win and percentage away win


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

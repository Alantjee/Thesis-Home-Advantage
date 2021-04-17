covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)


YellowCardDifference_covid <-  mean(covid_data$YellowCardDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(YellowCardDifference))
YellowCardDifference_noncovid <- mean(non_covid_data$YellowCardDifference)
FoulDifference_covid <- mean(covid_data$FoulDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(FoulDifference))
non_covid_data <- non_covid_data %>% filter(!is.na(FoulDifference))
FoulDifference_noncovid <- mean(non_covid_data$FoulDifference)
sd_yellow_covid <- sd(covid_data$YellowCardDifference)
sd_yellow_noncovid <- sd(non_covid_data$YellowCardDifference)
sd_fouls_noncovid <- sd(non_covid_data$FoulDifference)
sd_fouls_covid <- sd(covid_data$FoulDifference)

se_yellow_covid <- sd_home_covid/sqrt(length(covid_data))
se_fouls_covid <- sd_away_covid/sqrt(length(covid_data))
se_yellow_noncovid <- sd_home_noncovid/sqrt(length(non_covid_data))
se_fouls_noncovid <- sd_away_noncovid/sqrt(length(non_covid_data))

situation3 <- c("Diff yellow cards covid", "diff yellow cards pre covid", "diff fouls covid","diff fouls pre covid")
meanspread <- c(YellowCardDifference_covid , YellowCardDifference_noncovid,FoulDifference_covid ,FoulDifference_noncovid)
se <- c(se_home_covid, se_away_covid, se_home_noncovid, se_away_noncovid)
df_mean_spread <- cbind(situation, meanspread, se)
df_mean_spread <- data.frame(df_mean_spread)
str(df_mean_spread)
df_mean_spread$situation3 <- factor(situation3, levels = c("Diff yellow cards pre covid", "diff yellow cards covid", "diff fouls pre covid","diff fouls covid"))
levels(df_mean_win$situation3)
df_mean_win
plot_mean_card_foul_spread <- ggplot(df_mean_spread, aes(x = situation3, y = meanspread, 
                                         ymin = meanspread-se, ymax = meanspread+se)) +
  geom_bar(aes(color = situation), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = situation), width = 0.2) + 
  xlab("Cards and fouls distribution") +
  ylab("Spread") +
  ggtitle("Difference in fouls and cards between home and away teams before and after covid") +
  theme_minimal()
plot_mean_card_foul_spread


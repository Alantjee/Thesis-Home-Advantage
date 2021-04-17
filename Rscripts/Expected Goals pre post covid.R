covid_data <- covid_data %>% filter(!is.na(ExpectedGoalsDifference))
ExpectedGoalsDifference_covid <-  mean(covid_data$ExpectedGoalsDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(ExpectedGoalsDifference))
ExpectedGoalsDifference_noncovid <- mean(non_covid_data$ExpectedGoalsDifference)

sd_expectedgoals_covid <- sd(covid_data$ExpectedGoalsDifference)
sd_expectedgoals_noncovid <- sd(non_covid_data$ExpectedGoalsDifference)


se_expectedgoals_covid <- sd_expectedgoals_covid/sqrt(nrow(covid_data))
se_expectedgoals_noncovid <- sd_expectedgoals_noncovid/sqrt(nrow(non_covid_data))

Legend <- c("Expected Goals Difference pre Covid", "Expected Goals Difference Covid")
meanexpectedgoalsspread <- c(ExpectedGoalsDifference_noncovid , ExpectedGoalsDifference_covid)
se <- c(se_expectedgoals_noncovid, se_expectedgoals_covid)
df_meanexpectedgoalsspread  <- cbind(situation4, meanexpectedgoalsspread , se)
df_meanexpectedgoalsspread <- data.frame(df_meanexpectedgoalsspread)
df_meanexpectedgoalsspread$se <- as.numeric(df_meanexpectedgoalsspread$se)
df_meanexpectedgoalsspread$meanexpectedgoalsspread <- as.numeric(df_meanexpectedgoalsspread$meanexpectedgoalsspread)
df_meanexpectedgoalsspread$Legend <- factor(Legend, levels = c("Expected Goals Difference pre Covid", "Expected Goals Difference Covid"))
levels(df_percentage_points$Legend)

plot_meanexpectedgoalsspread <- ggplot(df_meanexpectedgoalsspread, aes(x = situation4, y = meanexpectedgoalsspread, 
                                                         ymin = meanexpectedgoalsspread - se, ymax = meanexpectedgoalsspread + se)) +
  geom_bar(aes(color = situation4), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = situation4), width = 0.2) + 
  xlab("Expected Goals") +
  ylab("Spread") +
  ggtitle("Difference in Expected Goals between home and away teams before and after covid") +
  theme_minimal()
plot_meanexpectedgoalsspread


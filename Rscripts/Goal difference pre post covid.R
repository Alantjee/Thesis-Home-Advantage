covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)


GoalDifference_covid <-  mean(covid_data$GoalDifference)
GoalDifference_noncovid <- mean(non_covid_data$GoalDifference)

sd_goals_covid <- sd(covid_data$GoalDifference)
sd_goals_noncovid <- sd(non_covid_data$GoalDifference)


se_goals_covid <- sd_goals_covid/sqrt(nrow(covid_data))
se_goals_noncovid <- sd_goals_noncovid/sqrt(nrow(non_covid_data))

Legend <- c("Goal Difference pre Covid", "Goal Difference Covid")
goalspread<- c(GoalDifference_noncovid , GoalDifference_covid)
se <- c(se_goals_noncovid, se_goals_covid)
df_goalspread  <- cbind(Legend, goalspread , se)
df_goalspread <- data.frame(df_goalspread)
df_goalspread$se <- as.numeric(df_goalspread$se)
df_goalspread$goalspread <- as.numeric(df_goalspread$goalspread)
df_goalspread$Legend <- factor(Legend, levels = c("Goal Difference pre Covid", "Goal Difference Covid"))
levels(df_goalspread$Legend)

plot_goalspread <- ggplot(df_goalspread, aes(x = Legend, y = goalspread, 
                                                                       ymin = goalspread - se, ymax = goalspread + se)) +
  geom_bar(aes(color = Legend), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = Legend), width = 0.2) + 
  xlab("Goals") +
  ylab("Spread") +
  ggtitle("Difference in Goals pre and post covid") +
  theme_minimal()
plot_goalspread
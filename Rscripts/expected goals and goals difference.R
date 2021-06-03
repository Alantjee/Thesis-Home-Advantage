#filter na weg en mean value
covid_data <- covid_data %>% filter(!is.na(ExpectedGoalsDifference))
ExpectedGoalsDifference_covid <-  mean(covid_data$ExpectedGoalsDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(ExpectedGoalsDifference))
ExpectedGoalsDifference_noncovid <- mean(non_covid_data$ExpectedGoalsDifference)

#sd expectedgoals
sd_expectedgoals_covid <- sd(covid_data$ExpectedGoalsDifference)
sd_expectedgoals_noncovid <- sd(non_covid_data$ExpectedGoalsDifference)

#se expectedgoals
se_expectedgoals_covid <- sd_expectedgoals_covid/sqrt(nrow(covid_data))
se_expectedgoals_noncovid <- sd_expectedgoals_noncovid/sqrt(nrow(non_covid_data))

#opnieuw data filteren om voor goal difference weer hele dataset te gebruiken
covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)

#mean goaldifference pre and post covid
GoalDifference_covid <-  mean(covid_data$GoalDifference)
GoalDifference_noncovid <- mean(non_covid_data$GoalDifference)

#sd covid
sd_goals_covid <- sd(covid_data$GoalDifference)
sd_goals_noncovid <- sd(non_covid_data$GoalDifference)

#se covid
se_goals_covid <- sd_goals_covid/sqrt(nrow(covid_data))
se_goals_noncovid <- sd_goals_noncovid/sqrt(nrow(non_covid_data))

#namen, mean values en se 
Legend <- c("Expected Goals Difference pre Covid", "Expected Goals Difference Covid", "Goal Difference pre Covid", "Goal Difference Covid")
allgoalspread<- c(ExpectedGoalsDifference_noncovid , ExpectedGoalsDifference_covid, GoalDifference_noncovid , GoalDifference_covid)
se <- c(se_expectedgoals_noncovid, se_expectedgoals_covid, se_goals_noncovid, se_goals_covid)

#tot dataframe maken
df_allgoalspread  <- cbind(Legend, allgoalspread , se)
df_allgoalspread <- data.frame(df_allgoalspread)
#convert to numeric format
df_allgoalspread$se <- as.numeric(df_allgoalspread$se)
df_allgoalspread$allgoalspread <- as.numeric(df_allgoalspread$allgoalspread)
df_allgoalspread$Legend <- factor(Legend, levels = c("Expected Goals Difference pre Covid", "Expected Goals Difference Covid","Goal Difference pre Covid", "Goal Difference Covid"))

#plot
plot_allgoalspread <- ggplot(df_allgoalspread, aes(x = Legend, y = allgoalspread, 
                                             ymin = allgoalspread - se, ymax = allgoalspread + se)) +
  geom_bar(aes(color = Legend), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = Legend), width = 0.2) + 
  xlab("Goals and Expected Goals") +
  ylab("Spread") +
  ggtitle("Difference in Goals pre and post covid") +
  theme_minimal()

plot_allgoalspread
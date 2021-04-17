points_home_covid <-  mean(covid_data$home_points)
points_home_noncovid <- mean(non_covid_data$home_points)
points_away_covid <- mean(covid_data$away_points)
points_away_noncovid <- mean(non_covid_data$away_points)
sd_home_covid <- sd(covid_data$home_points)

sd_away_covid <- sd(covid_data$away_points)
sd_home_noncovid <- sd(non_covid_data$home_points)
sd_away_noncovid <- sd(non_covid_data$away_points)
se_home_covid <- sd_home_covid/sqrt(length(covid_data))
se_away_covid <- sd_away_covid/sqrt(length(covid_data))
se_home_noncovid <- sd_home_noncovid/sqrt(length(non_covid_data))
se_away_noncovid <- sd_away_noncovid/sqrt(length(non_covid_data))

situation2 <- c("Average points home covid", "Average points away covid", "Average points home pre covid","Average points away pre covid")
meanpoints2 <- c(points_home_covid ,points_away_covid,points_home_noncovid ,points_away_noncovid)

se <- c(se_home_covid, se_away_covid, se_home_noncovid, se_away_noncovid)
df_mean_points <- cbind(situation2, meanpoints2, se)
df_mean_points <- data.frame(df_mean_points)
str(df_mean_points)
df_mean_points$situation2 <- factor(situation2, levels = c("Average Points home pre covid","Average Points Away pre covid","Average Points Home covid", "Average Points Away covid"))
levels(df_mean_points$situation2)
df_mean_points
plot_mean_points <- ggplot(df_mean_win, aes(x = situation, y = meanpoints2, 
                                         ymin = meanpoints2-se, ymax = meanpoints2+se)) +
  geom_bar(aes(color = situation), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = situation), width = 0.2) + 
  xlab("Home vs Away points") +
  ylab("Average points") +
  ggtitle("Points home and away pre and post covid") +
  theme_minimal()
plot_mean_points




 
covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)


percentage_points_home_covid <-  mean(covid_data$PercentagePointsHome)
percentage_points_home_noncovid <- mean(non_covid_data$PercentagePointsHome)
percentage_points_away_covid <- mean(covid_data$percentage_points_away)
percentage_points_away_noncovid <- mean(non_covid_data$percentage_points_away)
sd_home_covid <- sd(covid_data$PercentagePointsHome)
sd_away_covid <- sd(covid_data$percentage_points_away)
sd_home_noncovid <- sd(non_covid_data$PercentagePointsHome)
sd_away_noncovid <- sd(non_covid_data$percentage_points_away)
se_home_covid <- sd_home_covid/sqrt(nrow(covid_data))
se_away_covid <- sd_away_covid/sqrt(nrow(covid_data))
se_home_noncovid <- sd_home_noncovid/sqrt(nrow(non_covid_data))
se_away_noncovid <- sd_away_noncovid/sqrt(nrow(non_covid_data))

Legend <- c("Perc points home pre covid", "Perc points away pre covid", "Perc points home covid","Perc points away covid")
meanpoints5 <- c(percentage_points_home_noncovid ,percentage_points_away_noncovid,percentage_points_home_covid ,percentage_points_away_covid)

se <- c(se_home_covid, se_away_covid, se_home_noncovid, se_away_noncovid)
df_percentage_points <- cbind(Legend, meanpoints5, se)
df_percentage_points <- data.frame(df_percentage_points)
df_percentage_points$se <- as.numeric(df_percentage_points$se)
df_percentage_points$meanpoints5 <- as.numeric(df_percentage_points$meanpoints5)

str(df_percentage_points)
df_percentage_points$Legend <- factor(Legend, levels = c("Perc points home pre covid", "Perc points away pre covid", "Perc points home covid","Perc points away covid"))
levels(df_percentage_points$Legend)
df_percentage_points
plot_percentage_points <- ggplot(df_percentage_points, aes(x = Legend, y = meanpoints5, 
                                            ymin = meanpoints5-se, ymax = meanpoints5+se)) +
  geom_bar(aes(color = Legend), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = Legend), width = 0.2) + 
    xlab("Home vs Away points percentage") +
  ylab("Percentage points") +
  ggtitle("Percentage points pre and post covid") +
  theme_minimal()
plot_percentage_points

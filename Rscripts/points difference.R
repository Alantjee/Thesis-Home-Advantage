covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)


points_diff_covid <-  mean(covid_data$PointsDifference)
points_diff_noncovid <- mean(non_covid_data$PointsDifference)

sd_home_covid <- sd(covid_data$PointsDifference)
sd_home_noncovid <- sd(non_covid_data$PointsDifference)
se_home_covid <- sd_home_covid/sqrt(nrow(covid_data))
se_home_noncovid <- sd_home_noncovid/sqrt(nrow(non_covid_data))
Legend <- c("Points diff pre covid", "Points diffcovid")
meanpoints5 <- c(points_diff_noncovid ,points_diff_covid)

se <- c(se_home_noncovid, se_home_covid)
df_points <- cbind(Legend, meanpoints5, se)
df_points <- data.frame(df_percentage_points)
df_points$se <- as.numeric(df_percentage_points$se)
df_points$meanpoints5 <- as.numeric(df_percentage_points$meanpoints5)

str(df_points)
df_points$Legend <- factor(Legend, levels = c("Points diff pre covid", "Points diffcovid"))
levels(df_points$Legend)
df_points
plot_points <- ggplot(df_points, aes(x = Legend, y = meanpoints5, 
                                                           ymin = meanpoints5-se, ymax = meanpoints5+se)) +
  geom_bar(aes(color = Legend), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = Legend), width = 0.2) + 
  xlab("Home vs Away points percentage") +
  ylab("Percentage points") +
  ggtitle("Percentage points pre and post covid") +
  theme_minimal()
plot_points

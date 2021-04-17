yellowspread_covid <-  mean(covid_data$YellowCardDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(YellowCardDifference))
yellowspread_noncovid <- mean(non_covid_data$YellowCardDifference)

sd_yellowspread_covid <- sd(covid_data$YellowCardDifference)
sd_yellowspread_noncovid <- sd(non_covid_data$YellowCardDifference)

se_yellowspread_covid <- sd_yellowspread_covid/sqrt(nrow(covid_data))
se_yellowspread_noncovid <- sd_yellowspread_covid/sqrt(nrow(non_covid_data))


Legend <- c("Difference yellow Cards pre Covid", "Difference yellow Cards Covid")
yellowspread <- c(yellowspread_noncovid ,yellowspread_covid)

se <- c(se_yellowspread_covid, se_yellowspread_noncovid)
df_yellowspread <- cbind(yellowspread, Legend, se)
df_yellowspread <- data.frame(df_yellowspread)
df_yellowspread$se <- as.numeric(df_yellowspread$se)
df_yellowspread$yellowspread <- as.numeric(df_yellowspread$yellowspread)
str(df_yellowspread)
df_yellowspread$Legend <- factor(Legend, levels = c("Difference yellow Cards pre Covid", "Difference yellow Cards Covid"))
levels(df_yellowspread$Legend)
df_yellowspread
plot_yellowspread <- ggplot(df_yellowspread, aes(x = Legend, y = yellowspread, 
                                           ymin = yellowspread-se, ymax = yellowspread+se)) +
  geom_bar(aes(color = Legend), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = Legend), width = 0.2) + 
  xlab("yellow cards distribution") +
  ylab("Difference yellow Cards") +
  ggtitle("Difference yellow cards home and away pre and post covid") +
  theme_minimal()
plot_yellowspread

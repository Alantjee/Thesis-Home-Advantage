Foulspread_covid <-  mean(covid_data$FoulDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(FoulDifference))
Foulspread_noncovid <- mean(non_covid_data$FoulDifference)

sd_Foulspread_covid <- sd(covid_data$FoulDifference)
sd_Foulspread_noncovid <- sd(non_covid_data$FoulDifference)
nrow(covid_data)
se_Foulspread_covid <- sd_Foulspread_covid/sqrt(nrow(covid_data))
se_Foulspread_noncovid <- sd_Foulspread_covid/sqrt(nrow(non_covid_data))


Legend <- c("Difference Foul pre Covid", "Difference Foul Covid")
Foulspread <- c(Foulspread_noncovid ,Foulspread_covid)

se <- c(se_Foulspread_covid, se_Foulspread_noncovid)
df_Foulspread <- cbind(Foulspread, Legend, se)
df_Foulspread <- data.frame(df_Foulspread)
df_Foulspread$se <- as.numeric(df_Foulspread$se)
df_Foulspread$Foulspread <- as.numeric(df_Foulspread$Foulspread)
str(df_Foulspread)
df_Foulspread$Legend <- factor(Legend, levels = c("Difference Foul pre Covid", "Difference Foul Covid"))
levels(df_Foulspread$Legend)
df_Foulspread
plot_Foulspread <- ggplot(df_Foulspread, aes(x = Legend, y = Foulspread, 
                                           ymin = Foulspread-se, ymax = Foulspread+se)) +
  geom_bar(aes(color = Legend), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = Legend), width = 0.2) + 
  xlab("Foul cards distribution") +
  ylab("Difference Fouls") +
  ggtitle("Difference Fouls pre and post covid") +
  theme_minimal()
plot_Foulspread

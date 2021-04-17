redspread_covid <-  mean(covid_data$RedCardDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(RedCardDifference))
redspread_noncovid <- mean(non_covid_data$RedCardDifference)

sd_redspread_covid <- sd(covid_data$RedCardDifference)
sd_redspread_noncovid <- sd(non_covid_data$RedCardDifference)

se_redspread_covid <- sd_redspread_covid/sqrt(nrow(covid_data))
se_redspread_noncovid <- sd_redspread_covid/sqrt(nrow(non_covid_data))


Legend <- c("Difference Red Cards pre Covid", "Difference Red Cards Covid")
redspread <- c(redspread_noncovid ,redspread_covid)

se <- c(se_redspread_covid, se_redspread_noncovid)
df_redspread <- cbind(redspread, Legend, se)
df_redspread <- data.frame(df_redspread)
df_redspread$se <- as.numeric(df_redspread$se)
df_redspread$redspread <- as.numeric(df_redspread$redspread)
str(df_redspread)
df_redspread$Legend <- factor(Legend, levels = c("Difference Red Cards pre Covid", "Difference Red Cards Covid"))
levels(df_redspread$Legend)
df_redspread
plot_redspread <- ggplot(df_redspread, aes(x = Legend, y = redspread, 
                                                           ymin = redspread-se, ymax = redspread+se)) +
  geom_bar(aes(color = Legend), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = Legend), width = 0.2) + 
  xlab("Red cards distribution") +
  ylab("Difference Red Cards") +
  ggtitle("Difference red cards home and away pre and post covid") +
  theme_minimal()
plot_redspread


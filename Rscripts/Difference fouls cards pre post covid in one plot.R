covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)

yellowspread_covid <-  mean(covid_data$YellowCardDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(YellowCardDifference))
yellowspread_noncovid <- mean(non_covid_data$YellowCardDifference)

redspread_covid <-  mean(covid_data$RedCardDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(RedCardDifference))
redspread_noncovid <- mean(non_covid_data$RedCardDifference)

Foulspread_covid <-  mean(covid_data$FoulDifference)
non_covid_data <- non_covid_data %>% filter(!is.na(FoulDifference))
Foulspread_noncovid <- mean(non_covid_data$FoulDifference)

sd_yellowspread_covid <- sd(covid_data$YellowCardDifference)
sd_yellowspread_noncovid <- sd(non_covid_data$YellowCardDifference)

sd_redspread_covid <- sd(covid_data$RedCardDifference)
sd_redspread_noncovid <- sd(non_covid_data$RedCardDifference)

sd_Foulspread_covid <- sd(covid_data$FoulDifference)
sd_Foulspread_noncovid <- sd(non_covid_data$FoulDifference)

se_yellowspread_covid <- sd_yellowspread_covid/sqrt(nrow(covid_data))
se_yellowspread_noncovid <- sd_yellowspread_covid/sqrt(nrow(non_covid_data))

se_redspread_covid <- sd_redspread_covid/sqrt(nrow(covid_data))
se_redspread_noncovid <- sd_redspread_covid/sqrt(nrow(non_covid_data))

se_Foulspread_covid <- sd_Foulspread_covid/sqrt(nrow(covid_data))
se_Foulspread_noncovid <- sd_Foulspread_covid/sqrt(nrow(non_covid_data))

Legend <- c("Yellow Cards pre Covid", "Yellow Cards Covid", "Red Cards pre Covid", "Red Cards Covid", "Fouls pre Covid", "Fouls Covid")
refereespread <- c(yellowspread_noncovid ,yellowspread_covid, redspread_noncovid, redspread_covid, Foulspread_noncovid, Foulspread_covid)

se <- c(se_yellowspread_noncovid, se_yellowspread_covid, se_redspread_noncovid, se_redspread_covid, se_Foulspread_noncovid, se_Foulspread_covid)
df_refereespread <- cbind(refereespread, Legend, se)
df_refereespread <- data.frame(df_refereespread)
df_refereespread$se <- as.numeric(df_refereespread$se)
df_refereespread$refereespread <- as.numeric(df_refereespread$refereespread)
str(df_refereespread)
df_refereespread$Legend <- factor(Legend, levels = c("Yellow Cards pre Covid", "Yellow Cards Covid", "Red Cards pre Covid", "Red Cards Covid", "Fouls pre Covid", "Fouls Covid"))
df_refereespread
plot_refereespread <- ggplot(df_refereespread, aes(reorder(Legend, refereespread), y = refereespread, 
                                                 ymin = refereespread-se, ymax = refereespread+se)) +
  geom_bar(aes(color = Legend), stat = "identity", fill ="white") + 
  geom_errorbar(aes(color = Legend), width = 0.2) + 
  xlab("Cards and Fouls distribution") +
  ylab("Difference Yellow Cards, Fouls and Red Cards") +
  ggtitle("Difference Yellow Cards, Red Cards and Fouls pre and post covid") +
  theme_minimal()
plot_refereespread

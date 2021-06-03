#extra variabelen maken die de maand en jaar(09-2018 bijvoorbeeld) waarin de wedstrijd gespeeld is aangeeft
full_dataset_alan <- full_dataset_alan %>% mutate( month = floor_date(full_dataset_alan$Date, "month"))
#dataformat veranderen om de plots te kunnen maken per variabele.
aggregatered <- aggregate(full_dataset_alan["RedCardDifference"], by = full_dataset_alan["month"], mean)
aggregateyellow <- aggregate(full_dataset_alan["YellowCardDifference"], by = full_dataset_alan["month"], mean)
aggregatefouls <- aggregate(full_dataset_alan["FoulDifference"], by = full_dataset_alan["month"], mean)


#zelfde als hiervoor maar dan voor alle performance metrics; goals, punten 
aggregategoals <- aggregate(full_dataset_alan["GoalDifference"], by = full_dataset_alan["month"], mean)
aggregatepoints <- aggregate(full_dataset_alan["PointsDifference"], by = full_dataset_alan["month"], mean)

#yellow card time plot
time_series_yellowcard <- ggplot(data = aggregateyellow) + 
  geom_line(mapping = aes(x = month,y = YellowCardDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Yellow Card Difference over Time",x ="Time", y = "Yellow Card Difference")  + theme_minimal()  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
print(time_series_yellowcard)
yellow_card_time_plot <- time_series_yellowcard + annotate("text", x = as.Date("2020-03-01"), y = 0.5, label="Start Covid Pandemic", vjust = 0 )

yellow_card_time_plot

#red card time plot
time_series_redcard <- ggplot(data = aggregatered) + 
  geom_line(mapping = aes(x = month,y = RedCardDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Red Card Difference over Time",x =" Time", y = "Red Card Difference")  + theme_minimal()  

print(time_series_redcard)
red_card_time_plot <- time_series_redcard + annotate("text", x = as.Date("2020-03-01"), y = 0.3, label="Start Covid Pandemic", vjust = 0 )

red_card_time_plot

#fouls time plot

time_series_fouls<- ggplot(data = aggregatefouls) + 
  geom_line(mapping = aes(x = month,y = FoulDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Foul Difference over Time",x ="Time", y = "Foul  Differenc")  + theme_minimal()  

print(time_series_fouls)
fouls_time_plot <- time_series_fouls + annotate("text", x = as.Date("2020-03-01"), y = 1.5, label="Start Covid Pandemic", vjust = 0 )

fouls_time_plot

#goals time plot

time_series_goals <- ggplot(data = aggregategoals) + 
  geom_line(mapping = aes(x = month,y = GoalDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Goal Difference over Time",x ="Time", y = "Goal Difference")  + theme_minimal()  

print(time_series_goals)
goals_time_plot <- time_series_goals + annotate("text", x = as.Date("2020-03-01"), y = 0.6, label="Start Covid Pandemic", vjust = 0 )

goals_time_plot



#points time plot
time_series_points <- ggplot(data = aggregatepoints) + 
  geom_line(mapping = aes(x = month,y = PointsDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Points Difference over Time",x ="Time", y = "Points Difference")  + theme_minimal()  

print(time_series_points)
points_time_plot <- time_series_points + annotate("text", x = as.Date("2020-03-01"), y = 0.8, label="Start Covid Pandemic", vjust = 0 )
points_time_plot


#expected goals time plot
#eerst weer data weghalen voor expected goasl die missing is. 
full_dataset_alan <- full_dataset_alan[complete.cases(full_dataset_alan), ]

aggregateExpectedgoals <- aggregate(full_dataset_alan["ExpectedGoalsDifference"], by = full_dataset_alan["month"], mean)
time_series_expectedgoals <- ggplot(data = aggregateExpectedgoals) + 
  geom_line(mapping = aes(x = month,y = ExpectedGoalsDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Expected Goals Difference over Time",x ="Time", y = "Expected Goals Difference")  + theme_minimal()  

print(time_series_expectedgoals)
expectedgoals_time_plot <- time_series_expectedgoals + annotate("text", x = as.Date("2020-03-01"), y = 0.5, label="Start Covid Pandemic", vjust = 0 )

expectedgoals_time_plot

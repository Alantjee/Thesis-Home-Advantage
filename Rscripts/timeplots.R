
full_dataset_alan <- full_dataset_alan %>% mutate( month = floor_date(full_dataset_alan$Date, "month"))
aggregate1 <- aggregate(full_dataset_alan["RedCardDifference"], by = full_dataset_alan["month"], mean)
aggregate2 <- aggregate(full_dataset_alan["YellowCardDifference"], by = full_dataset_alan["month"], mean)
aggregate3 <- aggregate(full_dataset_alan["FoulDifference"], by = full_dataset_alan["month"], mean)
aggregatecards <- left_join(aggregate2, aggregate1, by = "month")
aggregateref <- left_join(aggregatecards, aggregate3, by = "month")

aggregate4 <- aggregate(full_dataset_alan["GoalDifference"], by = full_dataset_alan["month"], mean)
aggregate5 <- aggregate(full_dataset_alan["ExpectedGoalsDifference"], by = full_dataset_alan["month"], mean)
aggregate6 <- aggregate(full_dataset_alan["PointsDifference"], by = full_dataset_alan["month"], mean)
aggregategoals <- left_join(aggregate4, aggregate5, by = "month")
aggregateperformance <- left_join(aggregategoals, aggregate6, by = "month")

aggregate7 <- melt(aggregateref, "month")
time_series_all <- ggplot(data = aggregate7,aes(x =month , y = value, color = variable)) + 
  geom_line() + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Referee Decisions over Time",x ="Time", y = " Difference")  + theme_minimal()  

all_time_plot <- time_series_all + annotate("text", x = as.Date("2020-03-01"), y = 0.5, label="Start Covid Pandemic", vjust = 0 )

all_time_plot
#aggregate ref
colour <- c(  "yellow" = "YellowCardDifference" , "orange" = "Red Card Difference", "blue" = "Foul Difference" )
time_series_referee <- ggplot(data = aggregateref) + 
  geom_line(mapping = aes(x = month,y = YellowCardDifference, group = 1, colour = "yellow")) + 
  geom_line(mapping = aes(x = month, y = RedCardDifference, group = 1, colour = "orange")) + 
  geom_line(mapping = aes(x = month, y = FoulDifference, group = 1, colour = "blue")) + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Referee Decisions over time",x ="Time", y = "Difference", legend = colour)  + theme_minimal()  

referee_time_plot <- time_series_referee + annotate("text", x = as.Date("2020-03-01"), y = 1.5, label="Start Covid Pandemic", vjust = 0 )

referee_time_plot
#yellow cards
time_series_yellowcard <- ggplot(data = aggregate2) + 
  geom_line(mapping = aes(x = month,y = YellowCardDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Yellow Card Difference over Time",x ="Time", y = "Yellow Card Difference")  + theme_minimal()  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
print(time_series_yellowcard)
yellow_card_time_plot <- time_series_yellowcard + annotate("text", x = as.Date("2020-03-01"), y = 0.5, label="Start Covid Pandemic", vjust = 0 )

yellow_card_time_plot


#red cards
time_series_redcard <- ggplot(data = aggregate1) + 
  geom_line(mapping = aes(x = month,y = RedCardDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Red Card Difference over Time",x =" Time", y = "Red Card Difference")  + theme_minimal()  

print(time_series_redcard)
red_card_time_plot <- time_series_redcard + annotate("text", x = as.Date("2020-03-01"), y = 0.3, label="Start Covid Pandemic", vjust = 0 )

red_card_time_plot

#Fouls

time_series_fouls<- ggplot(data = aggregate3) + 
  geom_line(mapping = aes(x = month,y = FoulDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Foul Difference over Time",x ="Time", y = "Foul  Differenc")  + theme_minimal()  

print(time_series_fouls)
fouls_time_plot <- time_series_fouls + annotate("text", x = as.Date("2020-03-01"), y = 1.5, label="Start Covid Pandemic", vjust = 0 )

fouls_time_plot

#goals

time_series_goals <- ggplot(data = aggregate4) + 
  geom_line(mapping = aes(x = month,y = GoalDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Goal Difference over Time",x ="Time", y = "Goal Difference")  + theme_minimal()  

print(time_series_goals)
goals_time_plot <- time_series_goals + annotate("text", x = as.Date("2020-03-01"), y = 0.6, label="Start Covid Pandemic", vjust = 0 )

goals_time_plot



#points
time_series_points <- ggplot(data = aggregate6) + 
  geom_line(mapping = aes(x = month,y = PointsDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Points Difference over Time",x ="Time", y = "Points Difference")  + theme_minimal()  

print(time_series_points)
points_time_plot <- time_series_points + annotate("text", x = as.Date("2020-03-01"), y = 0.8, label="Start Covid Pandemic", vjust = 0 )
points_time_plot


#expected goals
full_dataset_alan <- full_dataset_alan[complete.cases(full_dataset_alan), ]

aggregate5 <- aggregate(full_dataset_alan["ExpectedGoalsDifference"], by = full_dataset_alan["month"], mean)
time_series_expectedgoals <- ggplot(data = aggregate5) + 
  geom_line(mapping = aes(x = month,y = ExpectedGoalsDifference, group = 1), color = "blue") + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Expected Goals Difference over Time",x ="Time", y = "Expected Goals Difference")  + theme_minimal()  

print(time_series_expectedgoals)
expectedgoals_time_plot <- time_series_expectedgoals + annotate("text", x = as.Date("2020-03-01"), y = 0.5, label="Start Covid Pandemic", vjust = 0 )

expectedgoals_time_plot

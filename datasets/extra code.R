

model_variables2 <- dummy_cols(model_variables$league)
modelall<- cbind(model_variables, model_variables2)
colnames(modelall) <- c("AverageAttendance", "OccupancyRate", "league", "YellowCardDifference", "RatingDifference", "AgeDifference", "RedCardDifference", "ImportanceDifference" , "covid", "GoalDifference", "PointsDifference", "FoulDifference", "ForeignersShareDifference" , "PercentagepointsHome", "VAR", "CornerDifference", "ShotsDifference", "ShotsTargetDifference", ".data_BarclaysPremierLeague", ".data_BelgianJupilerLeague", ".data_DutchEredivisie", ".data_FrenchLigue1", ".data_GermanBundesliga", ".data_ItalySerieA", ".data_PortugueseLiga", ".data_SpanishPrimeraDivision", ".data_TurkishTurkcellSuperlig") 

#mahal

#model_variables$RatingDifference <- scale(model_variables$RatingDifference, center = TRUE, scale = TRUE )[,]
#model_variables$ImportanceDifference <- scale(model_variables$ImportanceDifference, center = TRUE, scale = TRUE )[,]



#mahal <- mahalanobis(model_variables, 
#                   colMeans(model_variables),
#                  cov(model_variables))

#cutoff = qchisq(1-0.001, ncol(model_variables))
#table(mahal < cutoff)
#model_variables <- subset(model_variables, mahal< cutoff)
#nrow(model_variables)

#model_diagnostics <- augment(bpath)
#head(model_diagnostics)



x1 <- wilcox.test(covid_data$FTHG, non_covid_data$FTHG)
x1
x2 <- wilcox.test(covid_data$FTAG, non_covid_data$FTAG)
x2
x3 <-wilcox.test(covid_data$HY, non_covid_data$HY) 
x3
x4 <-wilcox.test(covid_data$AY, non_covid_data$AY)
x4
x5 <-wilcox.test(covid_data$HR, non_covid_data$HR)
x5
x6 <-wilcox.test(covid_data$AR, non_covid_data$AR)
x6
x7 <-wilcox.test(covid_data$HF, non_covid_data$HF)
x7
x8 <-wilcox.test(covid_data$AF, non_covid_data$AF)
x8
x9 <-chisq.test(covid_data$away_win, non_covid_data$away_win)
x9
x10 <-chisq.test(covid_data$home_win, non_covid_data$home_win)
x10
x11 <-wilcox.test(covid_data$HS, non_covid_data$HS)
x11
x12 <-wilcox.test(covid_data$AS, non_covid_data$AS)
x12
x13 <-wilcox.test(covid_data$xg1, non_covid_data$xg1)
x13
x14 <-wilcox.test(covid_data$xg2, non_covid_data$xg2)
x14
x15 <- wilcox.test(covid_data$home_points, non_covid_data$home_points)
x15
x16 <- wilcox.test(covid_data$away_points, non_covid_data$away_points)
x16
x17 <- wilcox.test(covid_data$PercentagePointsHome, non_covid_data$PercentagePointsHome)
x17
x18 <- wilcox.test(covid_data$percentage_points_away, non_covid_data$percentage_points_away)
x18
x19 <- wilcox.test(covid_data$HST, non_covid_data$HST)
x19
x20 <- wilcox.test(covid_data$AST, non_covid_data$AST)
x20
x21 <- wilcox.test(covid_data$diff_point, non_covid_data$diff_point)
x21
x22 <- wilcox.test(covid_data$YellowCardDifference, non_covid_data$YellowCardDifference)
x22
x23 <- wilcox.test(covid_data$RedCardDifference, non_covid_data$RedCardDifference)
x23
x24 <- wilcox.test(covid_data$FoulDifference, non_covid_data$FoulDifference)
x24
x25 <- wilcox.test(covid_data$GoalDifference, non_covid_data$GoalDifference)
x25






model <- ' FoulDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  + d5 * .data_BarclaysPremierLeague + d6 * .data_BelgianJupilerLeague + d7* .data_DutchEredivisie + d8* .data_FrenchLigue1 + d9* .data_GermanBundesliga + d10* .data_SpanishPrimeraDivision + d11* .data_TurkishTurkcellSuperlig + d12* .data_ItalySerieA + d13* .data_PortugueseLiga
           GoalDifference ~ cp * covid + b* FoulDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e5 * .data_BarclaysPremierLeague + e6 * .data_BelgianJupilerLeague + e7* .data_DutchEredivisie + e8* .data_FrenchLigue1 + e9* .data_GermanBundesliga + e10* .data_SpanishPrimeraDivision + e11* .data_TurkishTurkcellSuper lig + e12* .data_ItalySerieA + e13*.data_PortugueseLiga
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           moderated effect occupancy := b3
           total effect occupancy := (a3 * b)  + b3
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           total effect attendance := (a6 * b) + b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           total effect foreigners share := (a7 * b) + b7 
           moderated effect Age := b9
           total := cp + (a*b) 
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3
           counfounder Var := d4'
fitmodelmain <- sem(model, data = modelall, se="bootstrap", test="scaled.shifted",
                    estimator="DWLS", verbose = TRUE, bootstrap = 10)
summary(fitmodelmain, fit.measures = T,ci = T,  standardized = T, rsquare = T)
varTable(fitmodelmain)




model859 <- 'YellowCardDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference
           PointsDifference ~ cp * covid + b* YellowCardDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  
        
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           moderated effect occupancy := b3
           total effect occupancy := (a3 * b)  + b3
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           total effect attendance := (a6 * b) + b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           total effect foreigners share := (a7 * b) + b7 
           moderated effect Age := b9
           total := cp + (a*b) 
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3'
fitmodel859 <- sem(model859 ,data = model_variables, se = "bootstrap", bootstrap =10)
summary(fitmodel859, fit.measures = T,ci = T,  standardized = T, rsquare = T)




#SD.below := (cp + (a*b))  + b3*OccupancyRate.mean +b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))+ b3*(OccupancyRate.mean - sqrt(OccupancyRate.var))
#mean := (cp + (a*b))   + b3*OccupancyRate.mean +b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))
#SD.above := (cp + (a*b)) + (2* b3*(OccupancyRate.mean + sqrt(OccupancyRate.var)))



aggregate7 <- melt(aggregateref, "month")
time_series_all <- ggplot(data = aggregate7,aes(x =month , y = value, color = variable)) + 
  geom_line() + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Referee Decisions over Time",x ="Time", y = " Difference")  + theme_minimal()  

all_time_plot <- time_series_all + annotate("text", x = as.Date("2020-03-01"), y = 0.5, label="Start Covid Pandemic", vjust = 0 )

all_time_plot


#referee decisions plot
colour <- c(  "yellow" = "YellowCardDifference" , "orange" = "Red Card Difference", "blue" = "Foul Difference" )
time_series_referee <- ggplot(data = aggregateref) + 
  geom_line(mapping = aes(x = month,y = YellowCardDifference, group = 1, colour = "yellow")) + 
  geom_line(mapping = aes(x = month, y = RedCardDifference, group = 1, colour = "orange")) + 
  geom_line(mapping = aes(x = month, y = FoulDifference, group = 1, colour = "blue")) + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), linetype = 4, col = "red") +
  labs(title="Referee Decisions over time",x ="Time", y = "Difference", legend = colour)  + theme_minimal()  

referee_time_plot <- time_series_referee + annotate("text", x = as.Date("2020-03-01"), y = 1.5, label="Start Covid Pandemic", vjust = 0 )

referee_time_plot


aggregategoals <- left_join(aggregategoaldifference, aggregateExpectedgoals, by = "month")
aggregateperformance <- left_join(aggregategoals, aggregatepoints, by = "month")
                                  
                                  aggregatecards <- left_join(aggregateyellow, aggregatered, by = "month")
                                  aggregatereferee <- left_join(aggregatecards, aggregatefouls, by = "month")
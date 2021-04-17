library(sjPlot)
des <- c('YellowCardDifference', 'RedCardDifference', 
   'FoulDifference',  'AgeDifference',
   'ExpectedGoalsDifference',
   'ImportanceDifference',
   'RatingDifference',
   'OccupancyRate',
  'GoalDifference',
   'PercentagePointsHome',
   'ForeignersShareDifference',
  'AverageAttendance')

descriptivevariables = full_dataset_alan[, which(colnames(full_dataset_alan)%in%des)]
#descriptivevariables <- cbind(full_dataset_alan [, 4:5], full_dataset_alan[, 24:25], full_dataset_alan[, 30:34], full_dataset_alan[, 37:40], full_dataset_alan [, 45:48], full_dataset_alan[, 52:56], full_dataset_alan[, 61:62], full_dataset_alan[, 67:68] )
#descriptivevariables <- descriptivevariables[ , -which(names(descriptivevariables) %in% c("avg_age_away", "rating_diff", "xg_diff"))]

stargazer(descriptivevariables,
            type = "html",
            median = TRUE,
            title = "Descriptive Statistics", 
            #order=c("PercentagePointsHome", "ExpectedGoalsDifference", "GoalDifference", "ForeignersShareDifference", "AgeDifference", "OccupancyRate", "AverageAttendance", "YellowCardDifference", "FoulDifference", "RedCardDifference", "RatingDifference", "ImportanceDifference"),
            out="stats_descriptives.html")

#SPI <- SPI[2:23]
#SPI <- subset( SPI, select = -c(league_id, ))
#Data <- subset( Data, select = -a )
#results18_19 <-  results18_19[ , -which(names(results18_19) %in% c("time"))]
#df_results colnames(X) <- c("good", "better")

#write.csv(full_dataset_alan, "C:/Documents/thesis/datasets/final_dataset.csv", row.names = FALSE, sep = ";")


contingency <- as.table(as.matrix(meanwin))
chisq.test(contingency)
res <- prop.test(x = c(490, 400), n = c(500, 500))
contingency


win_home_covid <- mean(covid_data$home_win) 
win_home_noncovid <- mean(non_covid_data$home_win)
win_away_covid <- mean(covid_data$away_win)
win_away_noncovid <- mean(non_covid_data$away_win)


count_home_win <- count(covid_data$home_win)
prop.test(x = c(1210, 1014), n = c(2334, 1539),
          alternative = "less")
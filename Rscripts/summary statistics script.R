library(sjPlot)
des <- c('YellowCardDifference', 'RedCardDifference', 
   'FoulDifference',  'AgeDifference',
   'ExpectedGoalsDifference',
   'ImportanceDifference',
   'RatingDifference',
   'OccupancyRate',
  'GoalDifference',
   'PointsDifference',
    'ForeignersShareDifference',
  'AverageAttendance')

descriptivevariables = full_dataset_alan[, which(colnames(full_dataset_alan)%in%des)]
stargazer(descriptivevariables,
          type = "html",
          median = TRUE,
          title = "Descriptive Statistics",
          out = "descriptives.html")
stargazer(full_dataset_alan,
            type = "html",
            median = TRUE,
            title = "Descriptive Statistics full", 
            out="full_descriptives.html")

stargazer(covid_data,
          type = "html",
          median = TRUE,
          title = "Descriptive Statistics covid sample", 
          out="covid_descriptives.html")


stargazer(non_covid_data,
          type = "html",
          median = TRUE,
          title = "Descriptive Statistics pre-covid sample", 
          out="non_covid_descriptives.html")




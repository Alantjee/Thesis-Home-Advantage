model1 <- plm(PercentagePointsHome ~ FoulDifference + covid +AverageAttendance + OccupancyRate  + covid*AverageAttendance +ForeignersShareDifference + AgeDifference + covid*OccupancyRate +  + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR , data =full_dataset_alan, index = "league", model = "within")
summary(model1)
model2 <- lm(PercentagePointsHome ~ FoulDifference + covid +AverageAttendance + OccupancyRate  + covid*AverageAttendance +ForeignersShareDifference + AgeDifference + covid*OccupancyRate +  + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR + as.factor(league) -1 , data =full_dataset_alan)
summary(model2)

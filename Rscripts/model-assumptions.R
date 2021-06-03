path <- lm(GoalDifference~ YellowCardDifference +covid +AverageAttendance + OccupancyRate  + covid*AverageAttendance +ForeignersShareDifference + AgeDifference + covid*OccupancyRate +  + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference +  RatingDifference + VAR + as.factor(league) , data = model_variables)

apath <- lm(YellowCardDifference ~  covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid*AverageAttendance +covid*OccupancyRate + covid*ForeignersShareDifference + RatingDifference + ImportanceDifference + ShotsDifference + VAR + as.factor(league), data = model_variables)

#NCV test
ncvTest(path)
ncvTest(apath)

#variance inflation factors
vif(path)
vif(apath)

#plots homoscedasticity
par(mfrow = c(2, 2))
plot(path ,1)
plot(path, 3)

plot(apath,3)

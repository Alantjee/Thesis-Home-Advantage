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




path <- lm(GoalDifference~ YellowCardDifference +covid +AverageAttendance + OccupancyRate  + covid*AverageAttendance +ForeignersShareDifference + AgeDifference + covid*OccupancyRate +  + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference +  RatingDifference + VAR + as.factor(league) , data = model_variables)

apath <- lm(YellowCardDifference ~  covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid*AverageAttendance +covid*OccupancyRate + covid*ForeignersShareDifference + RatingDifference + ImportanceDifference + ShotsDifference + VAR + as.factor(league), data = model_variables)


ncvTest(path)
ncvTest(apath)
vif(path)
vif(apath)


par(mfrow = c(2, 2))
plot(path ,1)
plot(path, 3)

plot(apath,3)

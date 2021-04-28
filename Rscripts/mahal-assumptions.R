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




path <- lm(GoalDifference ~ FoulDifference +covid +AverageAttendance + OccupancyRate  + covid*AverageAttendance +ForeignersShareDifference + AgeDifference + covid*OccupancyRate +  + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR , data = model_variables)

apath <- lm(FoulDifference ~ covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid*AverageAttendance +covid*OccupancyRate + covid*ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR, data = model_variables)

bptest(apath)
gvmodel <- gvlma(path)
#gvmodel2 <- gvlma()
assumptions <- pander(gvmodel)
#assumptions2 <- pander()
library(car)
leveneTest(apath)
ncvTest(path)
ncvTest(apath)
vif(path)
vif(apath)
#model_diagnostics <- augment(bpath)
#head(model_diagnostics)

#summary(bpath)
#par(mfrow = c(2, 2))
#plot(bpath)
#plot(bpath, 1)
#plot(bpath, 2)
#plot(bpath, 3)
#plot(bpath, 4)
#plot(bpath, 5)
#plot(bpath, 6)

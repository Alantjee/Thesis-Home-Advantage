#SEM
set.seed(1234)
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("PointsDifference","league","CornerDifference","ShotsTargetDifference","ShotsDifference","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]

model_variables <- model_variables[complete.cases(model_variables), ]
model_variables$ForeignersShareDifference <- scale(model_variables$ForeignersShareDifference, center = TRUE, scale = TRUE)[,] #Scale returns a matrix so we have to make it a vector by indexing one column
model_variables$AverageAttendance <- scale(model_variables$AverageAttendance, center = TRUE, scale = TRUE)[,]
model_variables$AgeDifference <- scale(model_variables$AgeDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]

#model_variables$RatingDifference <- scale(model_variables$RatingDifference, center = TRUE, scale = TRUE )[,]
#model_variables$ImportanceDifference <- scale(model_variables$ImportanceDifference, center = TRUE, scale = TRUE )[,]



#mahal <- mahalanobis(model_variables, 
  #                   colMeans(model_variables),
   #                  cov(model_variables))

#cutoff = qchisq(1-0.001, ncol(model_variables))
#table(mahal < cutoff)
#model_variables <- subset(model_variables, mahal< cutoff)
#nrow(model_variables)




#path <- lm(PercentagePointsHome ~ FoulDifference +covid +AverageAttendance + OccupancyRate  + covid*AverageAttendance +ForeignersShareDifference + AgeDifference + covid*OccupancyRate +  + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR , data = model_variables)

#apath <- lm(FoulDifference ~ covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid*AverageAttendance +covid*OccupancyRate + covid*ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR, data = model_variables)

gvmodel <- gvlma()
gvmodel2 <- gvlma()
assumptions <- pander()
assumptions2 <- pander()

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

model.ref1 <- ' FoulDifference ~ a*covid + d1 * ImportanceDifference + d2 * RatingDifference + d3 * VAR
                GoalDifference ~ b*FoulDifference +c*covid + d2 * ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'


fit1 <- sem(model.ref1, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit1, standardized = T, fit.measures = T, rsq = T)


model.ref2 <- ' FoulDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                GoalDifference ~ b*FoulDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'


fit2 <- sem(model.ref2, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit2, standardized = T,fit.measures = T, rsq = T )


model.ref3 <- ' FoulDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                PointsDifference ~ b*FoulDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit3 <- sem(model.ref3, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit3, standardized = T,fit.measures = T, rsq = T )

model.ref4 <- ' FoulDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                PercentagePointsHome ~ b*FoulDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit4 <- sem(model.ref4, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit4, standardized = T, fit.measures = T, rsq = T)

model.ref5 <- ' YellowCardDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                GoalDifference ~ b*YellowCardDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit5 <- sem(model.ref5, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit5, standardized = T, fit.measures = T, rsq = T)

model.ref6 <- ' YellowCardDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                PercentagePointsHome ~ b*YellowCardDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit6 <- sem(model.ref6, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit6, standardized = T, fit.measures = T, rsq = T)


model.ref7 <- ' YellowCardDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                PointsDifference ~ b*YellowCardDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit7 <- sem(model.ref7, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit7, standardized = T, fit.measures = T, rsq = T)


model.ref8 <- ' RedCardDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                GoalDifference ~ b*RedCardDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit8 <- sem(model.ref8, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit8, standardized = T, fit.measures = T, rsq = T)


model.ref9 <- ' RedCardDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                PointsDifference ~ b*RedCardDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit9 <- sem(model.ref9, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit9, standardized = T, fit.measures = T, rsq = T)


model.ref10 <- 'RedCardDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                PercentagePointsHome ~ b*RedCardDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit10 <- sem(model.ref10, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit10, standardized = T, fit.measures = T, rsq = T)



semPaths(fit2, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
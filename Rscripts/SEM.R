#SEM

model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]

model_variables$ForeignersShareDifference <- scale(model_variables$ForeignersShareDifference, center = TRUE, scale = TRUE)[,] #Scale returns a matrix so we have to make it a vector by indexing one column
model_variables$AverageAttendance <- scale(model_variables$AverageAttendance, center = TRUE, scale = TRUE)[,]
model_variables$AgeDifference <- scale(model_variables$AgeDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
model_variables$RatingDifference <- scale(model_variables$RatingDifference, center = TRUE, scale = TRUE )[,]
model_variables$ImportanceDifference <- scale(model_variables$ImportanceDifference, center = TRUE, scale = TRUE )[,]
model_variables$covid <- as.factor(model_variables$covid)
model_variables$Crowdsize <- as.factor(model_variables$Crowdsize)
model_variables <- model_variables[complete.cases(model_variables), ]

mahal <- mahalanobis(model_variables, 
                     colMeans(model_variables),
                     cov(model_variables))

cutoff = qchisq(1-0.001, ncol(model_variables))
table(mahal < cutoff)
model_variables <- subset(model_variables, mahal< cutoff)
nrow(model_variables)

summary(gvmodel <- gvlma(bpath))
summary(model)
summary(bpath)
gvmodel2 <- gvlma(model)



model.ref <- ' FoulDifference ~ covid  + OccupancyRate + ForeignersShareDifference + covid * OccupancyRate + covid*ForeignersShareDifference +as.factor(Crowdsize) + as.factor(Crowdsize) *covid+  RatingDifference + ImportanceDifference + VAR
               GoalDifference ~ covid + OccupancyRate + Crowdsize + ForeignersShareDifference + AgeDifference + covid * OccupancyRate + covid*Crowdsize + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR + FoulDifference'



fit <- sem(model.ref, data = model_variables)
summary(fit)
summary(fit, standardized = T,fit.measures = T, rsq = T )

model_variables$covid <- as.factor(model_variables$covid)

model.ref2 <- ' FoulDifference ~ a*covid  + OccupancyRate + ForeignersShareDifference + covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                GoalDifference ~ b*FoulDifference +c*covid + OccupancyRate  + ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  + covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := a*b
                direct := c
                total := c + (a*b)'

model.ref3 <- ' FoulDifference ~ a*covid  + OccupancyRate + ForeignersShareDifference + covid * OccupancyRate + covid*ForeignersShareDifference +as.factor(Crowdsize) + as.factor(Crowdsize) *covid + RatingDifference + ImportanceDifference + VAR
                PercentagePointsHome ~ b*FoulDifference +c*covid + cdash1* OccupancyRate + cdashCrowdsize + ForeignersShareDifference + AgeDifference + covid * OccupancyRate + covid*Crowdsize + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := a*b
                direct := c
                total := c + (a*b)'

model.ref4 <- ' YellowCardDifference ~ a*covid  + OccupancyRate + ForeignersShareDifference + covid * OccupancyRate + covid*ForeignersShareDifference +as.factor(Crowdsize) + as.factor(Crowdsize) *covid+  RatingDifference + ImportanceDifference + VAR
                PercentagePointsHome ~ b*YellowCardDifference +c*covid + OccupancyRate + Crowdsize + ForeignersShareDifference + AgeDifference + covid * OccupancyRate + covid*Crowdsize + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := a*b
                direct := c
                total := c + (a*b)'

model.ref5 <- ' YellowCardDifference ~ a*covid  + OccupancyRate + ForeignersShareDifference + covid * OccupancyRate + covid*ForeignersShareDifference +as.factor(Crowdsize) + as.factor(Crowdsize) *covid+  RatingDifference + ImportanceDifference + VAR
                GoalDifference ~ b*YellowCardDifference +c*covid + OccupancyRate + Crowdsize + ForeignersShareDifference + AgeDifference + covid * OccupancyRate + covid*Crowdsize + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := a*b
                direct := c
                total := c + (a*b)'
str(model_variables)
summary(model_variables)
model_variables$covid <- as.numeric(model_variables$covid)

model_variables$Crowdsize <- as.numeric(model_variables$Crowdsize)
fit2 <- sem(model.ref2, data = model_variables)
fitsum <- summary(fit2, standardized = T,fit.measures = T, rsq = T )
fit2_boot <- sem(fit2, model_variables, se = "bootstrap")
summary(fit2_boot)
fit3 <- sem(model.ref3, data = model_variables)
summary(fit3, standardized = T,fit.measures = T, rsq = T )

fit4 <- sem(model.ref4, data = model_variables)
summary(fit4, standardized = T, fit.measures = T, rsq = T)

fit5 <- sem(model.ref5, data = model_variables)
summary(fit5, standardized = T, fit.measures = T, rsq = T)
semTable(fit2, file = "OutputSem2", type = "html")
semTable(fitsum, file = "OutputSem", type = "html")
semTable(fit3, file = "OutputSem3", type = "html")
semTable(fit4, file = "OutputSem4", type = "html")
semTable(fit5, file = "OutputSem5", type = "html")
semTable(list(fit, fit3, fit4, fit5), file = "OutputSemall", type = "html")

semPaths(fit2, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
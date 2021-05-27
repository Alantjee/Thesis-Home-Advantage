#SEM
set.seed(1234)
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("PointsDifference","league","CornerDifference","ShotsTargetDifference","ShotsDifference","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]

model_variables <- model_variables[complete.cases(model_variables), ]
model_variables$ForeignersShareDifference <- scale(model_variables$ForeignersShareDifference, center = TRUE, scale = TRUE)[,] #Scale returns a matrix so we have to make it a vector by indexing one column
model_variables$AverageAttendance <- scale(model_variables$AverageAttendance, center = TRUE, scale = TRUE)[,]
model_variables$AgeDifference <- scale(model_variables$AgeDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
model_variables$league <- as.numeric(factor(model_variables$league))



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


model.ref5 <- ' YellowCardDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                GoalDifference ~ b*YellowCardDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := -1*a*b
                direct := c
                total := c + (a*b)'

fit5 <- sem(model.ref5, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit5, standardized = T, fit.measures = T, rsq = T)



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


model.ref9 <- ' RedCardDifference ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR + ShotsDifference
                PointsDifference ~ b*RedCardDifference +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
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
                total := cp + (a*b)  +  (a7 * b) + b7 + (a6 * b) + b6 + (a3 * b)  + b3 + b9
                confounder Rating  := d1
                confounder Importance := d2
                confounder Shots := d3
                counfounder Var := d4
                confounder league := d6'

fit9 <- sem(model.ref9, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fit9, standardized = T, fit.measures = T, rsq = T)


semPaths(fit2, what='std', nCharNodes=6, sizeMan=10,
         edge.label.cex=1.25, curvePivot = TRUE, fade=FALSE)
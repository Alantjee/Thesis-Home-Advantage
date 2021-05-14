set.seed(1234)
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("ShotsDifference","CornerDifference","ShotsTargetDifference","highOccupancyRate", "lowOccupancyRate","league", "ShotsDifference", "PointsDifference","league","CornerDifference","ShotsTargetDifference","ShotsDifference","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]

model_variables <- model_variables[complete.cases(model_variables), ]
model_variables$ForeignersShareDifference <- scale(model_variables$ForeignersShareDifference, center = TRUE, scale = TRUE)[,] #Scale returns a matrix so we have to make it a vector by indexing one column
model_variables$AverageAttendance <- scale(model_variables$AverageAttendance, center = TRUE, scale = TRUE)[,]
model_variables$AgeDifference <- scale(model_variables$AgeDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
#model_variables = cbind(model_variables, dummy.code(model_variables$league))

model_variables$league <- as.numeric(factor(model_variables$league))



#laatste model, deze werkt het beste
model6 <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR + d5 * as.factor(league) 
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference  + d4* VAR + d5 * as.factor(league)  
           
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
#fitmodel6 <- sem(model6 ,data = model_variables, se = "robust.cluster", bootstrap = 100, cluster = "league")
#summary(fitmodel6, fit.measures = T, standardized = T, rsquare = T)
#fitmodel6 <- sem(model6, data = model_variables, ordered = "league" , se = "bootstrap",test="scaled.shifted",
                 #estimator="DWLS", verbose=TRUE,  bootstrap = 100)
fitmodel6 <- sem(model6, data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel6, fit.measures = T, standardized = T, rsquare = T)
#d5 * Barclays Premier League + d6 * Spanish Primera Division + d7 * Italy Serie A + d8 * French Ligue 1 + d9 * Turkish Turkcell Super Lig + d10 * German Bundesliga + d11* Portuguese Liga + d12 * Dutch Eredivisie + d13 * Belgian Jupiler League
model7 <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  + d6* as.factor(league)
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference  + d4* VAR + d6* as.factor(league)
           
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
           total := cp + (a*b) +  (a7 * b) + b7 + (a6 * b) + b6 + (a3 * b)  + b3 + b9
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3
           counfounder Var := d4
           confounder league := d6'
fitmodel7 <- sem(model7 ,data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel7, fit.measures = T, standardized = T, rsquare = T)


model8 <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR + d6* as.factor(league)
           PointsDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference  + d4* VAR + d6* as.factor(league)
           
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
fitmodel8 <- sem(model8 ,data = model_variables, se = "bootstrap", bootstrap = 5000)
summary(fitmodel8, fit.measures = T, standardized = T, rsquare = T)

#semPaths(fitmodel5, "path", sizeMan = 5, asize = 2.5, edge.label.position = .6, edge.label.cex = 0.8, edge.color = "black", layout = "tree")
#parameterestimates(fitmodel6)




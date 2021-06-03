firstmodel <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  
           PointsDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR 
           
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
           counfounder Var := d4
           '
fitfirstmodel <- sem(firstmodel ,data = model_variables, se = "bootstrap", bootstrap = 5000)
summary(fitfirstmodel, fit.measures = T, ci = T,standardized = T, rsquare = T)



modelrobust <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR 
           
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
           counfounder Var := d4
           '
fitmodelrobust <- sem(modelrobust ,data = model_variables, se = "bootstrap", bootstrap = 5000)
summary(fitmodelrobust, fit.measures = T, ci = T,standardized = T, rsquare = T)





modelrobust2 <- 'YellowCardDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR 
           PointsDifference ~ cp * covid + b* YellowCardDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR 
           
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
           counfounder Var := d4
           '
fitmodelrobust2 <- sem(modelrobust2 ,data = model_variables, se = "bootstrap", bootstrap = 5000)
summary(fitmodelrobust2, fit.measures = T,ci = T,  standardized = T, rsquare = T)







modelrobust3 <- 'YellowCardDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR 
           GoalDifference ~ cp * covid + b* YellowCardDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR 
           
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
           counfounder Var := d4
           '
fitmodelrobust3 <- sem(modelrobust3 ,data = model_variables, se = "bootstrap", bootstrap =5000)
summary(fitmodelrobust3, fit.measures = T,ci = T,  standardized = T, rsquare = T)


modelsimpleslope <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR
           PointsDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR 
           
           OccupancyRate ~ OccupancyRate.mean*1
           # variance of centered occupancy (moderator; for use in simple slopes)
           OccupancyRate ~~ OccupancyRate.var*OccupancyRate
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           total effect attendance := (a6 * b) + b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           total effect foreigners share := (a7 * b) + b7 
           moderated effect Age := b9
           Low := cp + (a*b) + b3*(OccupancyRate.mean - sqrt(OccupancyRate.var))
           mean := cp + (a*b) + b3*OccupancyRate.mean
           high := cp + (a*b) + b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3
           counfounder Var := d4
           '

fitmodelsimpleslope <- sem(modelsimpleslope ,data = model_variables, se = "bootstrap", bootstrap = 5000, fixed.x = FALSE)
summary(fitmodelsimpleslope)



modelrobust4 <- 'Refereebias =~   FoulDifference + YellowCardDifference + RedCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           RedCardDifference ~~ RedCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR 
           PointsDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR 
           
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
fitmodelrobust4 <- sem(modelrobust4, data = model_variables, se = "bootstrap", bootstrap = 5000)
summary(fitmodelrobust4, fit.measures = T,ci = T,  standardized = T, rsquare = T)


modelsimpleslope2 <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR 
           
           OccupancyRate ~ OccupancyRate.mean*1
           # variance of centered occupancy (moderator; for use in simple slopes)
           OccupancyRate ~~ OccupancyRate.var*OccupancyRate
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           total effect attendance := (a6 * b) + b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           total effect foreigners share := (a7 * b) + b7 
           moderated effect Age := b9
           Low := cp + (a*b) + b3*(OccupancyRate.mean - sqrt(OccupancyRate.var))
           mean := cp + (a*b) + b3*OccupancyRate.mean
           high := cp + (a*b) + b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3
           counfounder Var := d4
           '

fitmodelsimpleslope2 <- sem(modelsimpleslope2 ,data = model_variables, se = "bootstrap", bootstrap = 5000, fixed.x = FALSE)
summary(fitmodelsimpleslope2)








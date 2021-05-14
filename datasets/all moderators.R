
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("league", "ShotsDifference", "PointsDifference","league","CornerDifference","ShotsTargetDifference","ShotsDifference","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]
model_variables <- model_variables[complete.cases(model_variables), ]

model <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  + d6* as.factor(league)
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference  + d4* VAR + d6* as.factor(league)
           
           OccupancyRate ~ OccupancyRate.mean*1
           
           # variance of centered occupancy (moderator; for use in simple slopes)
           OccupancyRate ~~ OccupancyRate.var*OccupancyRate
           AgeDifference ~ AgeDifference.mean*1
           
           # variance of centered occupancy (moderator; for use in simple slopes)
           AgeDifference ~~ AgeDifference.var*AgeDifference
           AverageAttendance ~ AverageAttendance.mean*1
           
           # variance of centered occupancy (moderator; for use in simple slopes)
           AverageAttendance ~~ AverageAttendance.var*AverageAttendance
           ForeignersShareDifference ~ ForeignersShareDifference.mean*1
           
           # variance of centered occupancy (moderator; for use in simple slopes)
           ForeignersShareDifference ~~ ForeignersShareDifference.var*ForeignersShareDifference
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           SD.below := (cp + (a*b)  +  (a7 * b) + b7 + (a6 * b) + b6 + (a3 * b)  + b9 )+ b3*(OccupancyRate.mean - sqrt(OccupancyRate.var))
           mean := (cp + (a*b)  +  (a7 * b) + b7 + (a6 * b) + b6 + (a3 * b)  + b9 ) + b3*OccupancyRate.mean 
           SD.above := (cp + (a*b)  +  (a7 * b) + b7 + (a6 * b) + b6 + (a3 * b)  + b9 ) +  b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           total effect attendance := (a6 * b) + b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           total effect foreigners share := (a7 * b) + b7 
           moderated effect Age := b9
           total := cp + (a*b)  +  (a7 * b) + b7 + (a6 * b) + b6 + (a3 * b)  + b9 + b3
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3
           counfounder Var := d4
           confounder league := d6'
fitmodel <- sem(model ,data = model_variables, se = "bootstrap", bootstrap =100, fixed.x = FALSE)

summary(fitmodel)

set.seed(1234)

#variabelen die ik in het model gebruik uit algehele dataset halen
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c( "ShotsDifference", "PointsDifference","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference"))]

#missing observations voor Importance eruit
model_variables <- model_variables[complete.cases(model_variables), ]

#cronbach's alpha
Refereebias <- select(model_variables, FoulDifference, YellowCardDifference, RedCardDifference)
alpha(Refereebias)

Refereebias2 <- select(model_variables, FoulDifference, YellowCardDifference)
alpha(Refereebias2)

#standardize en mean center
model_variables$ForeignersShareDifference <- scale(model_variables$ForeignersShareDifference, center = TRUE, scale = TRUE)[,] 
model_variables$AverageAttendance <- scale(model_variables$AverageAttendance, center = TRUE, scale = TRUE)[,]
model_variables$AgeDifference <- scale(model_variables$AgeDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]

#mainmodel
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


#robustness check goals
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




#robustnesscheck yellow cards
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






#2e robustness check yellow cards
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

#red cards included in latent construct
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


#simple slope
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





#simple slope with goals as DV
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








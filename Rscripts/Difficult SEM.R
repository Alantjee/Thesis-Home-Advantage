set.seed(1234)
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("ShotsDifference","CornerDifference","ShotsTargetDifference","highOccupancyRate", "lowOccupancyRate","league", "ShotsDifference", "PointsDifference","league","CornerDifference","ShotsTargetDifference","ShotsDifference","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]

model_variables <- model_variables[complete.cases(model_variables), ]
model_variables$ForeignersShareDifference <- scale(model_variables$ForeignersShareDifference, center = TRUE, scale = TRUE)[,] #Scale returns a matrix so we have to make it a vector by indexing one column
model_variables$AverageAttendance <- scale(model_variables$AverageAttendance, center = TRUE, scale = TRUE)[,]
model_variables$AgeDifference <- scale(model_variables$AgeDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
model_variables$league <- as.numeric(as.factor(model_variables$league))
str(model_variables)
#model_variables$RatingDifference <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
#model_variables$ImportanceDifference <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
#model_variables$ShotsDifference <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
#modelall<-cbind(model_variables, model_variables2)

modelmain <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference 
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  
           
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
           confounder Shots := d3'
fitmodelmain <- sem(modelmain, data = model_variables, se="bootstrap", bootstrap = 5)
summary(fitmodelmain, fit.measures = T,ci = T,  standardized = T, rsquare = T)


model <- ' YellowCardDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid +  a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference + d4* VAR + d6 * as.factor(league)
           PointsDifference ~ cp * covid + b* YellowCardDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e3 * VAR + e4 * as.factor(league)
           
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
           confounder Shots := d3'



fitmodel <- sem(model, data = model_variables, se="bootstrap", bootstrap = 5)
summary(fitmodel, fit.measures = T,ci = T,  standardized = T, rsquare = T)


Refereebias <- select(model_variables, FoulDifference, YellowCardDifference, RedCardDifference)
alpha(Refereebias)

Refereebias2 <- select(model_variables, FoulDifference, YellowCardDifference)
alpha(Refereebias2)

model2 <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  + d6* as.factor(league)
           PointsDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e6* as.factor(league)
           
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
           confounder league := d6'
fitmodel2 <- sem(model2 ,data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel2, fit.measures = T, ci = T,standardized = T, rsquare = T)

model3 <- 'YellowCardDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR + d6* as.factor(league)
           PointsDifference ~ cp * covid + b* YellowCardDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e6* as.factor(league)
           
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
           confounder league := d6'
fitmodel3 <- sem(model3 ,data = model_variables, se = "bootstrap", bootstrap = 5000)
summary(fitmodel3, fit.measures = T,ci = T,  standardized = T, rsquare = T)

model4 <- 'YellowCardDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR + d6* as.factor(league)
           GoalDifference ~ cp * covid + b* YellowCardDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e6* as.factor(league)
           
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
           confounder league := d6'
fitmodel4 <- sem(model4 ,data = model_variables, se = "bootstrap", bootstrap = 5000)
summary(fitmodel4, fit.measures = T,ci = T,  standardized = T, rsquare = T)





model4 <- 'YellowCardDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR + d6* as.factor(league)
           GoalDifference ~ cp * covid + b* YellowCardDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e6* as.factor(league)
           
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
           confounder league := d6'
fitmodel4 <- sem(model4 ,data = model_variables, se = "bootstrap", bootstrap =100)
summary(fitmodel4, fit.measures = T,ci = T,  standardized = T, rsquare = T)
model5 <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  + d6* as.factor(league)
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e6* as.factor(league)
           
           OccupancyRate ~ OccupancyRate.mean*1
           # variance of centered occupancy (moderator; for use in simple slopes)
           OccupancyRate ~~ OccupancyRate.var*OccupancyRate
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           #SD.below := (cp + (a*b))  + b3*OccupancyRate.mean +b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))+ b3*(OccupancyRate.mean - sqrt(OccupancyRate.var))
           #mean := (cp + (a*b))   + b3*OccupancyRate.mean +b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))
           #SD.above := (cp + (a*b)) + (2* b3*(OccupancyRate.mean + sqrt(OccupancyRate.var)))
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
           confounder league := d6'

fitmodel5 <- sem(model5 ,data = model_variables, se = "bootstrap", bootstrap = 5000, fixed.x = FALSE)
summary(fitmodel5)


model6 <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  + d6* as.factor(league)
           PointsDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e6* as.factor(league)
           
           OccupancyRate ~ OccupancyRate.mean*1
           # variance of centered occupancy (moderator; for use in simple slopes)
           OccupancyRate ~~ OccupancyRate.var*OccupancyRate
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           #SD.below := (cp + (a*b))  + b3*OccupancyRate.mean +b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))+ b3*(OccupancyRate.mean - sqrt(OccupancyRate.var))
           #mean := (cp + (a*b))   + b3*OccupancyRate.mean +b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))
           #SD.above := (cp + (a*b)) + (2* b3*(OccupancyRate.mean + sqrt(OccupancyRate.var)))
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
           confounder league := d6'

fitmodel6 <- sem(model6 ,data = model_variables, se = "bootstrap", bootstrap = 5000, fixed.x = FALSE)
summary(fitmodel6)

model7 <- 'Refereebias =~   FoulDifference + YellowCardDifference + RedCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           RedCardDifference ~~ RedCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR + d5 * as.factor(league) 
           PointsDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e5 * as.factor(league)  
           
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
fitmodel7 <- sem(model7, data = model_variables, se = "bootstrap", bootstrap = 5000)
summary(fitmodel7, fit.measures = T,ci = T,  standardized = T, rsquare = T)

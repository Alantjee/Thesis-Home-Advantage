set.seed(1234)
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("league", "ShotsDifference", "PointsDifference","league","CornerDifference","ShotsTargetDifference","ShotsDifference","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]

model_variables <- model_variables[complete.cases(model_variables), ]
model_variables$ForeignersShareDifference <- scale(model_variables$ForeignersShareDifference, center = TRUE, scale = TRUE)[,] #Scale returns a matrix so we have to make it a vector by indexing one column
model_variables$AverageAttendance <- scale(model_variables$AverageAttendance, center = TRUE, scale = TRUE)[,]
model_variables$AgeDifference <- scale(model_variables$AgeDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
#model_variables = cbind(model_variables, dummy.code(model_variables$league))
str(model_variables)

model_variables$league <- as.numeric(factor(model_variables$league))
#model_variables <- model_variables["league", ordered]
str(model_variables)
model0 <- 'FoulDifference ~ a*covid + a2*ForeignersShareDifference + a3* ForeignersShareDifference:covid + a4*AverageAttendance + a5* AverageAttendance:covid  +a6 * OccupancyRate + a7 * covid:OccupancyRate + d1 * RatingDifference + d2 * ImportanceDifference + d3 * VAR
           GoalDifference ~ cp * covid + b* FoulDifference +b2*ForeignersShareDifference + b3* ForeignersShareDifference:covid + b4 * AgeDifference + b5*AgeDifference:covid + b6*AverageAttendance + b7* AverageAttendance:covid + b8*OccupancyRate + b9*OccupancyRate:covid + d1 * RatingDifference + d2 * ImportanceDifference + d3 * VAR 
           
           Direct := cp
           indirect := a*b
          # moderated mediation effect occupancy:= (a3 * b) 
           #moderated effect occupancy := b3
           #moderated mediation effect attendance:= (a6 * b) 
           #moderated effect attendance :=  b6
           #moderated mediation effect foreigners share := (a7 * b) 
           #moderated effect foreigners share := b7
           #moderated effect Age := b9
           total := cp + (a*b) #+ (a3*b) #+ (a6*b) + (a7*b) + b9 + b7 + b6 + b3
           confounder Rating  := d1
           confounder IMportance := d2
           confounder VAR := d3'

fitmodel0 <- sem(model0 ,data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel0, fit.measures = T, standardized = T, rsquare = T)

#zelfde als simpele model maar dan alle effecten van moderators etc erbij gezet, ook shotsdifference als control voor fouldifference en zonder var

model1 <- 'FoulDifference ~ a*covid + a2 * OccupancyRate + a3 * OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference
           GoalDifference ~ cp * covid + b* FoulDifference + b2 * OccupancyRate + b3 * OccupancyRate:covid  + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference  
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           moderated effect occupancy := b3
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           moderated effect Age := b9
           total := cp + (a*b) + (a3*b) + (a6*b) + (a7*b) + b9 + b7 + b6 + b3
           confounder Rating  := d1
           confounder IMportance := d2
           confounder Shots := d3'

fitmodel1 <- sem(model1 ,data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel1, fit.measures = T, standardized = T, rsquare = T)

#zonder shots difference en met var
model2 <- 'FoulDifference ~ a*covid + a2 * OccupancyRate + a3 * OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference  + d4 * VAR
           GoalDifference ~ cp * covid + b* FoulDifference + b2 * OccupancyRate + b3 * OccupancyRate:covid + + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference + d4 * VAR  
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           moderated effect occupancy := b3
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           moderated effect Age := b9
           total := cp + (a*b) + (a3*b) + (a6*b) + (a7*b) + b3 + b6 + b7 + b9
           confounder Rating  := d1
           confounder Importance := d2
           confounder var := d4'

fitmodel2 <- sem(model2 ,data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel2, fit.measures = T, standardized = T, rsquare = T)

#minder goede factor oplossing maar wel allebei de factors dominance en refereebias   geen var

model3 <- 'Refereebias =~  HY + AY + HR + AR + HF + AF
           Dominance =~  HS + AS + HST + AST + HC + AC
           
           Refereebias ~ a*covid + a2 * OccupancyRate + a3 * OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * Dominance
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3 * OccupancyRate:covid + + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference  
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           moderated effect occupancy := b3
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           moderated effect Age := b9
           total := cp + (a*b) + (a3*b) + (a6*b) + (a7*b) + b3 + b6 + b7 + b9
           confounder Rating  := d1
           confounder Importance := d2
           confounder Dominance := d3'

fitmodel3 <- sem(model3 ,data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel3, fit.measures = T, standardized = T, rsquare = T)

#goede factor for referebias maar geen dominance factor   wel var maar geen shots asl control

model4 <- 'Refereebias =~  FoulDifference + RedCardDifference + YellowCardDifference
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           RedCardDifference ~~ RedCardDifference
           YellowCardDifference ~~ YellowCardDifference
          
           Refereebias ~ a*covid + a2 * OccupancyRate + a3 * OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference +  d4* VAR
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3 * OccupancyRate:covid + + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference  + d4* VAR
           
           Direct := cp
           indirect := a*b
           moderated mediation effect occupancy:= (a3 * b) 
           moderated effect occupancy := b3
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           moderated effect Age := b9
           total := cp + (a*b) 
           confounder Rating  := d1
           confounder Importance := d2
           counfounder Var := d4'
fitmodel4 <- sem(model4 ,data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel4, fit.measures = T, standardized = T, rsquare = T)

#vorige plus shots difference als control in eerste equatie 
model5 <- 'Refereebias =~  FoulDifference + RedCardDifference + YellowCardDifference
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           RedCardDifference ~~ RedCardDifference
           YellowCardDifference ~~ YellowCardDifference
          
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + d1 * RatingDifference + d2 * ImportanceDifference  + d4* VAR
           
           Direct := cp
           indirect := a*b
          # moderated mediation effect occupancy:= (a3 * b) 
          # moderated effect occupancy := b3
           moderated mediation effect attendance:= (a6 * b) 
           moderated effect attendance :=  b6
           moderated mediation effect foreigners share := (a7 * b) 
           moderated effect foreigners share := b7
           moderated effect Age := b9
           total := cp + (a*b) 
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3
           counfounder Var := d4'
fitmodel5 <- sem(model5 ,data = model_variables, se = "bootstrap", bootstrap = 100)
summary(fitmodel5, fit.measures = T, standardized = T, rsquare = T)

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
           total := cp + (a*b) 
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3
           counfounder Var := d4
           confounder league := d6'
fitmodel7 <- sem(model7 ,data = model_variables, se = "bootstrap", bootstrap = 1000)
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
           total := cp + (a*b) 
           confounder Rating  := d1
           confounder Importance := d2
           confounder Shots := d3
           counfounder Var := d4
           confounder league := d6'
fitmodel8 <- sem(model8 ,data = model_variables, se = "bootstrap", bootstrap = 1000)
summary(fitmodel8, fit.measures = T, standardized = T, rsquare = T)

semPaths(fitmodel5, "path", sizeMan = 5, asize = 2.5, edge.label.position = .6, edge.label.cex = 0.8, edge.color = "black", layout = "tree")
parameterestimates(fitmodel6)



model_variables2 <- dummy_cols(model_variables$league)
modelall<- cbind(model_variables, model_variables2)
colnames(modelall) <- c("AverageAttendance", "OccupancyRate", "league", "YellowCardDifference", "RatingDifference", "AgeDifference", "RedCardDifference", "ImportanceDifference" , "covid", "GoalDifference", "PointsDifference", "FoulDifference", "ForeignersShareDifference" , "PercentagepointsHome", "VAR", "CornerDifference", "ShotsDifference", "ShotsTargetDifference", ".data_BarclaysPremierLeague", ".data_BelgianJupilerLeague", ".data_DutchEredivisie", ".data_FrenchLigue1", ".data_GermanBundesliga", ".data_ItalySerieA", ".data_PortugueseLiga", ".data_SpanishPrimeraDivision", ".data_TurkishTurkcellSuperlig") 







model <- ' FoulDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference +  d4* VAR  + d5 * .data_BarclaysPremierLeague + d6 * .data_BelgianJupilerLeague + d7* .data_DutchEredivisie + d8* .data_FrenchLigue1 + d9* .data_GermanBundesliga + d10* .data_SpanishPrimeraDivision + d11* .data_TurkishTurkcellSuperlig + d12* .data_ItalySerieA + d13* .data_PortugueseLiga
           GoalDifference ~ cp * covid + b* FoulDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  + e4* VAR + e5 * .data_BarclaysPremierLeague + e6 * .data_BelgianJupilerLeague + e7* .data_DutchEredivisie + e8* .data_FrenchLigue1 + e9* .data_GermanBundesliga + e10* .data_SpanishPrimeraDivision + e11* .data_TurkishTurkcellSuper lig + e12* .data_ItalySerieA + e13*.data_PortugueseLiga
           
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
fitmodelmain <- sem(model, data = modelall, se="bootstrap", test="scaled.shifted",
                    estimator="DWLS", verbose = TRUE, bootstrap = 10)
summary(fitmodelmain, fit.measures = T,ci = T,  standardized = T, rsquare = T)
varTable(fitmodelmain)




model859 <- 'YellowCardDifference ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference
           PointsDifference ~ cp * covid + b* YellowCardDifference + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference  
        
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
fitmodel859 <- sem(model859 ,data = model_variables, se = "bootstrap", bootstrap =10)
summary(fitmodel859, fit.measures = T,ci = T,  standardized = T, rsquare = T)




#SD.below := (cp + (a*b))  + b3*OccupancyRate.mean +b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))+ b3*(OccupancyRate.mean - sqrt(OccupancyRate.var))
#mean := (cp + (a*b))   + b3*OccupancyRate.mean +b3*(OccupancyRate.mean + sqrt(OccupancyRate.var))
#SD.above := (cp + (a*b)) + (2* b3*(OccupancyRate.mean + sqrt(OccupancyRate.var)))



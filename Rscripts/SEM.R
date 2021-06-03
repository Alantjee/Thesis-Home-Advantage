modelll <- 'Refereebias =~   FoulDifference + YellowCardDifference 
           
           Refereebias ~~ Refereebias
           FoulDifference ~~ FoulDifference
           YellowCardDifference ~~ YellowCardDifference
           
           
           Refereebias ~ a*covid + a2* OccupancyRate + a3*OccupancyRate:covid + a4 * AverageAttendance + a5 * ForeignersShareDifference + a6 * AverageAttendance:covid + a7 * ForeignersShareDifference:covid +  d1 * RatingDifference + d2 * ImportanceDifference + d3 * ShotsDifference 
           GoalDifference ~ cp * covid + b* Refereebias + b2 * OccupancyRate + b3* OccupancyRate:covid + b4 * AverageAttendance + b5 * ForeignersShareDifference + b6 * AverageAttendance:covid + b7 * ForeignersShareDifference:covid + b8 * AgeDifference + b9 * AgeDifference:covid + e1 * RatingDifference + e2 * ImportanceDifference'  
           
modelbasic <- 'FoulDifference ~ a*covid + d1*RatingDifference   
               PointsDifference ~ cp* covid + b* FoulDifference +e1*RatingDifference '

fitmodelbasic <- sem(modelbasic, data = model_variables, se="bootstrap", bootstrap = 5)
summary(fitmodelbasic, fit.measures = T,ci = T,  standardized = T, rsquare = T)

str(model_variables)

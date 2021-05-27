#confrimatory factor analysis
set.seed(1234)
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("league","HS","AS", "HST", "AST", "HC", "AC", "HY","AY","HR","AR","HF","AF","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]
model_variables <- model_variables[complete.cases(model_variables), ]
x <- round(cor(model_variables[, sapply(model_variables, is.numeric)],
    use = "complete.obs", method = "pearson"), 2)
y <- round(cov(model_variables[, sapply(model_variables, is.numeric)], 2)
view(x)
model_variables$FoulDifference <- scale(model_variables$FoulDifference, center = TRUE, scale = TRUE)[,] #Scale returns a matrix so we have to make it a vector by indexing one column
model_variables$YellowCardDifference <- scale(model_variables$YellowCardDifference, center = TRUE, scale = TRUE)[,]
model_variables$RedCardDifference <- scale(model_variables$RedCardDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
model_variables$HS <- scale(model_variables$HS, center = TRUE, scale = TRUE)[,]
model_variables$HST <- scale(model_variables$HST, center = TRUE, scale = TRUE)[,]
model_variables$AST <- scale(model_variables$AST, center = TRUE, scale = TRUE)[,]
model_variables$AS <- scale(model_variables$AS,center = TRUE, scale = TRUE)[,]
model_variables$HC <- scale(model_variables$HC, center = TRUE, scale = TRUE)[,]
model_variables$AC <- scale(model_variables$AC, center = TRUE, scale = TRUE)[,]
m1a  <- ' ref =~ FoulDifference + YellowCardDifference + RedCardDifference'
onefac3items_a <- cfa(m1a, data=full_dataset_alan) 

model <- 'Refereebias =~  FoulDifference + RedCardDifference + YellowCardDifference 
          Dominance =~ HS + HC + HST
          Refereebias ~~ Refereebias
          FoulDifference~~FoulDifference
          YellowCardDifference~~YellowCardDifference
          RedCardDifference~~RedCardDifference'



fit <- cfa(model, data = full_dataset_alan)
summary(fit, standardized = T)
fitMeasures(fit, c("cfi", "rmsea", "srmr"))

semPaths(fit, what = "paths", whatLabels = "stand", rotation = 1)
model.ref2 <- ' Refereebias ~ a*covid  + OccupancyRate + AverageAttendance +ForeignersShareDifference + covid:AverageAttendance +covid:OccupancyRate + covid:ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR
                GoalDifference ~ b*Refereebias +c*covid +AverageAttendance + OccupancyRate  + covid:AverageAttendance +ForeignersShareDifference + AgeDifference + covid:OccupancyRate +  covid:AgeDifference + covid:ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR 
                 
                indirect := a*b
                direct := c
                total := c + (a*b)'
fit2 <- sem(model.ref2, data = model_variables)

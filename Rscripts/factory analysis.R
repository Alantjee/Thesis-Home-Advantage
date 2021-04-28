set.seed(1234)
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("HY","AY","HF","HST","AF","HR","AR","AST","HS","AS","HC","AC","ShotsDifference","ShotsTargetDifference","CornerDifference","AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]

scale(model_variables)
model_variables$HF <- scale(model_variables$FoulDifference, center = TRUE, scale = FALSE)[,]
model_variables$ShotsDifference <- scale(model_variables$ShotsDifference,center = TRUE,  scale = TRUE)[,]
model_variables$YellowCardDifference <- scale(model_variables$YellowCardDifference, center = TRUE, scale = TRUE)[,]
model_variables$RedCardDifference <- scale(model_variables$RedCardDifference, center = TRUE, scale = TRUE)[,]
model_variables$ShotsTargetDifference <- scale(model_variables$ShotsTargetDifference, center = TRUE, scale = TRUE)[,]
model_variables$CornerDifference <- scale(model_variables$CornerDifference, center = TRUE, scale = TRUE)[,]

model_variables <- model_variables[complete.cases(model_variables), ]
mahal <- mahalanobis(model_variables, 
                     colMeans(model_variables),
                     cov(model_variables))
cutoff = qchisq(1-0.001, ncol(model_variables))
table(mahal < cutoff)
model_variables <- subset(model_variables, mahal< cutoff)
nrow(model_variables)

model <- ' Refereebias =~RedCardDifference + YellowCardDifference + FoulDifference
           Dominance =~  ShotsDifference + ShotsTargetDifference + CornerDifference'
fit <- cfa(model, data = model_variables)
summary(fit, standardized = T, rsq = T)
fitMeasures(fit, c("rmsea", "srmr", "cfi"))



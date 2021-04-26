#opdracht 22/04/2021
mediate <- mediation::mediate 
set.seed(1234)
model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome", "HS", "AS"))]
model_variables <- model_variables[complete.cases(model_variables), ]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
model_variables$RatingDifference <- scale(model_variables$RatingDifference, center = TRUE, scale = TRUE)[,]
model_variables$ImportanceDifference <- scale(model_variables$ImportanceDifference, center = TRUE, scale = TRUE)[,] 

mahal <- mahalanobis(model_variables, 
                     colMeans(model_variables),
                     cov(model_variables))

cutoff = qchisq(1-0.001, ncol(model_variables))
table(mahal < cutoff)
model_variables <- subset(model_variables, mahal< cutoff)
nrow(model_variables)


model1 <- lm(GoalDifference ~ covid + RatingDifference + ImportanceDifference , data = model_variables)
model2 <- lm(GoalDifference ~ covid + OccupancyRate + covid*OccupancyRate + RatingDifference + ImportanceDifference , data = model_variables)
model3 <- lm(FoulDifference ~ covid + RatingDifference + ImportanceDifference , data = model_variables)

apath <- lm(FoulDifference ~ covid  + OccupancyRate + covid * OccupancyRate + RatingDifference + ImportanceDifference ,  data = model_variables)
bpath <- lm(GoalDifference ~ covid + OccupancyRate + covid * OccupancyRate + ImportanceDifference + RatingDifference + FoulDifference, data = model_variables)
summary(apath)
summary(bpath)

a1 = apath$coefficients[2]
a2 =apath$coefficients[3]
a3 =apath$coefficients[6]
b = bpath$coefficients[6]
c3 = bpath$coefficients[7]
SEa1 = coef(summary(apath))[, "Std. Error"][2]
SEa2 = coef(summary(apath))[, "Std. Error"][3]
SEa3 = coef(summary(apath))[, "Std. Error"][6]
SEc3 = coef(summary(bpath))[, "Std. Error"][7] 
SEb = coef(summary(bpath))[, "Std. Error"][6]

zscore = (a3*b)/(sqrt((b^2*SEa3^2) + (a3^2*SEb^2)+(SEa3*SEb)))  
zscore = z
pnorm(abs(zscore), lower.tail = F)*2

total = cpath$coefficients[2]
direct = bpath$coefficients[2]
indirect = a3*b + c3
totalindirect = indirect - c3

z = 0.0598/ 0.05617809
z2 <- z^2
e = -0.717*z
f = 0.416*z2
a = e-f

#bootstrapping
indirectsaved = function(dataset, random){
  d = dataset[random, ]
  apath <- lm(FoulDifference ~ covid  + OccupancyRate + covid * OccupancyRate + RatingDifference + ImportanceDifference ,  data = d)
  bpath <- lm(GoalDifference ~ covid + OccupancyRate + covid * OccupancyRate + ImportanceDifference + RatingDifference + FoulDifference, data = d)
  x = apath$coefficients[6] * bpath$coefficients[6]
  return(x)
}

library(boot)
bootresults = boot(data = model_variables,
                   statistic = indirectsaved, 
                   R = 1000)
bootresults
summary(Model4)

mod.med <- ' FoulDifference ~ a*covid + OccupancyRate + covid:OccupancyRate + RatingDifference +   ImportanceDifference + VAR
             GoalDifference ~ b*FoulDifference + c*covid + OccupancyRate + covid:OccupancyRate  + RatingDifference +  ImportanceDifference + VAR 
             
            indirect := a*b
            direct := c
            total := c + (a*b)'
          x <-  
            '
             Indirect effect crowd support on Goal Difference := a1 * b
             Indirect effect Occupancy on Goal Difference := a2 * b
             Indirect effect interaction crowd support and Occupancy on Goal Difference := a3 * b
             Total effect of crowd support and Occupancy := (a3 * b) + c3
             Difference direct and indirect effect crowd support and Occupancy := (a3*b) - c3'


model5 <- sem(mod.med , data = model_variables)
summary(model5, standardized = T,fit.measures = T, rsq = T )
stargazer(model1, model2, model3, apath, bpath, type = "html", title = "Models", out = "models.html")
semTable(model4, file = "modelSem",type = "html")

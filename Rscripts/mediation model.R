#chapter 4 thesis
library(knitr)
require(gvlma)
library(haven)
library(trafo)


#assumptions
#1   Linear relationship
vif(fit)
summary(gvmodel <- gvlma(bpath))
summary(model)
summary(bpath)
gvmodel2 <- gvlma(model)
gvmodel3 <- gvlma(cpath)
assumptions <- pander(gvmodel)
assumptions
assumptions2 <- pander(model2)
assumptions3 <- pander(gvmodel3)
assumptions4 <- assumptions(bpath)
model <- trafo_lm(bpath)

plot(model)
model2 <- model(object = bpath, trafo = "logshiftopt",
                          + method = "skew")

diagnostics(model)
summary(model_variables)
write.table(gvmodel, file = "assumptions.txt")
assumptions <- pander(gvmodel)
stargazer(gvmodel,type = "html")
str(model_variables)
summary(gvmodel)

model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("AverageAttendance","GoalDifference","covid","VAR","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]
str(model_variables)
model_variables <-  full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("AverageAttendance","ImportanceDifference","GoalDifference","covid","VAR","Crowdsize","RatingDifference","ImportanceDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]
correlation_variables <- full_dataset_alan[, which(names(full_dataset_alan) %>% c("AverageAttendance","ImportanceDifference","GoalDifference","covid","VAR","Crowdsize","RatingDifference","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]

model_variables$Crowdsize <- as.factor(model_variables$Crowdsize)
str(model_variables)
summary(model_variables)
model_variables <- model_variables[complete.cases(model_variables), ]

mahal <- mahalanobis(model_variables, 
                     colMeans(model_variables),
                     cov(model_variables))

cutoff = qchisq(1-0.001, ncol(model_variables))
table(mahal < cutoff)
model_variables <- subset(model_variables, mahal< cutoff)
nrow(model_variables)
summary(mediation_model1)
structure()
correl = cor()
correl
symnum(correl)


#assumptions check
random = rchisq(nrow(noout), 7)
fake = lm(random ~., data = noout)
standardized = rstudent(fake)
fitted = scale(fake$fitted.values)
qqnorm(standardized)
abline(0,1)

hist(standardized)
plot(fitted, standardized)
abline(0,0)
abline(v=0)
library(caTools)
#2  Multivariate normailty
ks.test(full_dataset_alan$YellowCardDifference, y='pnorm',alternative='two.sided')


normality <- function(x){
  ad.test(x)
}
sapply(model_variables, normality)

#3  no or little multicolinearity

#4   no auto-correlation
#5  Homoscedasticity 

  bartlett.test(YellowCardDifference ~covid, data = model_variables )
  bartlett.test(YellowCardDifference ~OccupancyRate, data = model_variables )
  bartlett.test(YellowCardDifference ~AverageAttendance, data = model_variables )
  
   #bartlett.test(YellowCardDifference ~ Covid, OccupancyRate, ForeignersShareDifference, AverageAttendance,  RatingDifference, ImportanceDifference, VAR),  data = model_variables)
sapply(model_variables, bartlett)
# sample size  at least 20 cases per iv   makkelijk gehaald
cols <- names(model_variables)
bartlett.test
all_test <- lapply(cols, function(x) 
  bartlett.test(reformulate("YellowCardDifference", x), data = model_variables))
str(model_variables)
model_variables$covid <- as.factor(model_variables$covid)
model_variables$VAR <- as.factor(model_variables$VAR)
# linear relationship
#scatterplots

# Simple Scatterplot

plot(RedCardDifference, covid, main="Scatterplot Example",
     xlab="Red cards ", ylab="covid ", pch=19)

library(mediation)
library(broom)

#standardize data  

model_variables$ForeignersShareDifference <- scale(model_variables$ForeignersShareDifference, center = TRUE, scale = TRUE)[,] #Scale returns a matrix so we have to make it a vector by indexing one column
model_variables$AverageAttendance <- scale(model_variables$AverageAttendance, center = TRUE, scale = TRUE)[,]
model_variables$AgeDifference <- scale(model_variables$AgeDifference, center = TRUE, scale = TRUE)[,]
model_variables$OccupancyRate <- scale(model_variables$OccupancyRate, center = TRUE, scale = TRUE)[,]
model_variables$RatingDifference <- scale(model_variables$RatingDifference, center = TRUE, scale = TRUE )[,]
model_variables$ImportanceDifference <- scale(model_variables$ImportanceDifference, center = TRUE, scale = TRUE )[,]

model_variables$FoulDifference <- scale(model_variables$FoulDifference, center = TRUE, scale = TRUE )[,]
full_dataset_alan_standardized <- full_dataset_alan %>% mutate_if(is.numeric, scale)
full_dataset_alan_standardized <- full_dataset_alan_standardized %>% mutate_if(is.integer, scale)
#2 regression equations 

model_variables <- full_dataset_alan[ , which(names(full_dataset_alan) %in%  c("GoalDifference","RatingDifference","AverageAttendance", "ExpectedGoalsDifference","Crowdsize", "ImportanceDifference","VAR", "covid","VAR","RatingDifference","RedCardDifference", "FoulDifference", "ImportanceDifference ","AgeDifference", "ForeignersShareDifference", "OccupancyRate","YellowCardDifference", "RedCardDifference", "FoulDifference", "PercentagePointsHome"))]
library(mblm)
model.k = mblm(Calories ~ Sodium,
               data=Data)
str(model_variables)
mediation_model1 <- lm(FoulDifference ~ covid  + OccupancyRate + ForeignersShareDifference + covid * OccupancyRate + covid*ForeignersShareDifference +as.factor(Crowdsize) + as.factor(Crowdsize) *covid+  RatingDifference + ImportanceDifference + VAR,  data = full_dataset_alan_standardized)

mediation_model2 <- gam(FoulDifference ~ s(covid  + OccupancyRate + ForeignersShareDifference +  covid * OccupancyRate + covid*ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR),  data = train)
mediation_model3 <-gam(FoulDifference ~ s(covid  + OccupancyRate + ForeignersShareDifference +  covid * OccupancyRate + covid*ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR),  data = train)
maineffectmodel <- glm(home_win ~ covid + AgeDifference + as.factor(crowd) + OccupancyRate + ForeignersShareDifference + covid * AgeDifference + covid * as.factor(crowd) + covid * OccupancyRate + covid*ForeignersShareDifference + RatingDifference + ImportanceDifference + VAR + YellowCardDifference + FoulDifference,  data = full_dataset_alan_standardized, family = "binomial")
str(model_variables$Crowdsize)
model_variables$Crowdsize <- as.factor(model_variables$Crowdsize)
model_variables$covid <- as.factor(model_variables$covid)
cpath <- lm(GoalDifference ~ covid + OccupancyRate + as.factor(Crowdsize) + ForeignersShareDifference + AgeDifference + covid * OccupancyRate + covid*as.factor(Crowdsize) + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR, data = model_variables)
summary(cpath)
apath <- lm(FoulDifference ~ covid  + OccupancyRate + ForeignersShareDifference + covid * OccupancyRate + covid*ForeignersShareDifference +as.factor(Crowdsize) + as.factor(Crowdsize) *covid+  RatingDifference + ImportanceDifference + VAR,  data = model_variables)
bpath <- lm(GoalDifference ~ covid + OccupancyRate + Crowdsize + ForeignersShareDifference + AgeDifference + covid * OccupancyRate + covid*Crowdsize + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR + FoulDifference, data = model_variables)
summary(apath)
summary(bpath)
summary(model_variables$GoalDifference)
str(model_variables)
model_variables$covid <- as.factor(model_variables$covid)
a = apath$coefficients[2]
b = bpath$coefficients[10]
SEa = coef(summary(apath))[, "Std. Error"][2]
SEb = coef(summary(bpath))[, "Std. Error"][10]
model_variables <- model_variables %>% model_variables$GoalDifference != -13
zscore = (a*b)/(sqrt((b^2*SEa^2) + (a^2*SEb^2)+(SEa*SEb)))
zscore
pnorm(abs(zscore), lower.tail = F)*2

total = cpath$coefficients[2]
direct = bpath$coefficients[2]
indirect = a*b

total;direct;indirect
str(model_variables$Crowdsize)
model_variables$Crowdsize <- as.factor(model_variables$Crowdsize)
#bootstrapping
indirectsaved = function (dataset, random) {
  d = dataset[random, ]
  apath = lm(FoulDifference ~ covid  + OccupancyRate + ForeignersShareDifference + covid * OccupancyRate + covid*ForeignersShareDifference +as.factor(Crowdsize) + as.factor(Crowdsize) *covid+  RatingDifference + ImportanceDifference + VAR,  data = full_dataset_alan_standardized)
  bpath =  lm(PercentagePointsHome ~ covid + OccupancyRate + as.factor(Crowdsize) + ForeignersShareDifference + AgeDifference + covid * OccupancyRate + covid*as.factor(Crowdsize) + covid*AgeDifference + covid*ForeignersShareDifference + ImportanceDifference + RatingDifference + VAR + FoulDifference, data = full_dataset_alan_standardized)
  indirect = apath$coefficients[2] * bpath$coefficients[10]
  return(indirect)
  }

library(boot)
bootresults = boot(data = full_dataset_alan_standardized,
                   statistic = indirectsaved,
                   R = 1000)
bootresults

boot.ci(bootresults,
        conf = .95,
        type = "norm")
stargazer(apath, bpath, type = "text",title = "moderated mediation", out = "moderatedmediation.docx")

model_diagnostics <- augment(bpath)
head(model_diagnostics)

summary(bpath)
par(mfrow = c(2, 2))
plot(bpath)
plot(bpath, 1)
plot(bpath, 2)
plot(bpath, 3)
plot(bpath, 4)
plot(bpath, 5)
plot(bpath, 6)

library(ggfortify)
autoplot(mediation_model1)

model_diagnostics %>%
  top_n(3, wt = .cooksd)
call:
  lm(YellowCardDifference ~ covid  + OccupancyRate + ForeignersShareDifference + covid * OccupancyRate + covid*ForeignersShareDifference +AverageAttendance + covid*AverageAttendance+  RatingDifference + ImportanceDifference + VAR,  data = model_variables)
model_variables <- model_variables[-c(797, 5239, 7073), ]

library(mgcv)
# Build the model
model <- gam(medv ~ s(lstat), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)

str(train)
summary(train)
str(full_dataset_alan$covid)
full_dataset_alan$covid <- as.factor(full_dataset_alan$covid)
summary(mediation_model1)
summary(maineffectmodel)



m2a  <- ' f1  =~  HS+ AS + HST +  AST +  xg_diff +  shots_ratio_home +  shots_ratio_away +  goal_diff +  diff_point
          f2 =~ YellowCardDifference + RedCardDifference + FoulDifference
          f1 ~~ 0*f2'
m2a  <- ' f1  =~   HY + AY + HR + AR + HF + AF  '
onefac <- cfa(m2a, data = full_dataset_alan_standardized,std.lv=TRUE)

summary(onefac, fit.measures=TRUE,standardized=TRUE)
my.scores <- factor.scores(full_dataset_alan_standardized, onefac, method="tenBerge")
fs <- my.scores$scores
full_dataset_alan_standardized <- cbind(full_dataset_alan,fs) 








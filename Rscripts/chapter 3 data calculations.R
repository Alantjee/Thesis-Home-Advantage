#dit is voor hoofdstuk 3, eerst split ik dataset in voor en na covid
covid_data <- full_dataset_alan %>% filter(covid == 1)
non_covid_data <- full_dataset_alan %>% filter(covid != 1)

#mann-whitney test voor alle variabelen, heb ze 
significanceYellowcardDifference <- wmwTest(covid_data$YellowCardDifference, non_covid_data$YellowCardDifference, alternative = c("two.sided", "less", "greater"))
significanceRedcardDifference <- wmwTest(covid_data$RedCardDifference, non_covid_data$RedCardDifference, alternative = c("two.sided", "less", "greater"))
ignificanceFoulDifference <- wmwTest(covid_data$FoulDifference, non_covid_data$FoulDifference, alternative = c("two.sided", "less", "greater"))
significanceGoalDifference <- wmwTest(covid_data$GoalDifference, non_covid_data$GoalDifference, alternative = c("two.sided", "less", "greater"))
significancePercentagepointsHome <- wmwTest(covid_data$PercentagePointsHome, non_covid_data$PercentagePointsHome, alternative = c("two.sided", "less", "greater"))
significanceHomeYellow <- wmwTest(covid_data$HY, non_covid_data$HY, alternative = c("two.sided", "less", "greater"))
significanceAwayYellow <- wmwTest(covid_data$AY, non_covid_data$AY, alternative = c("two.sided", "less", "greater"))
significanceHomeRed <- wmwTest(covid_data$HR, non_covid_data$HR, alternative = c("two.sided", "less", "greater"))
significanceAwayRed <- wmwTest(covid_data$AR, non_covid_data$AR, alternative = c("two.sided", "less", "greater"))
significanceAwayfouls <- wmwTest(covid_data$AF, non_covid_data$AF, alternative = c("two.sided", "less", "greater"))
significanceHomeFouls <- wmwTest(covid_data$HF, non_covid_data$HF, alternative = c("two.sided", "less", "greater"))
significanceHomeGoals <- wmwTest(covid_data$FTHG, non_covid_data$FTHG, alternative = c("two.sided", "less", "greater"))
significanceAwayGoals <- wmwTest(covid_data$FTAG, non_covid_data$FTAG, alternative = c("two.sided", "less", "greater"))
significanceHomePoints <- wmwTest(covid_data$home_points, non_covid_data$home_points, alternative = c("two.sided", "less", "greater"))
significanceAwayPoints <- wmwTest(covid_data$away_points, non_covid_data$away_points, alternative = c("two.sided", "less", "greater"))
significanceHomeShots <- wmwTest(covid_data$HS, non_covid_data$HS, alternative = c("two.sided", "less", "greater"))
significanceAwayShots <- wmwTest(covid_data$AS, non_covid_data$AS, alternative = c("two.sided", "less", "greater"))
significanceHomeShotsTarget <- wmwTest(covid_data$HST, non_covid_data$HST, alternative = c("two.sided", "less", "greater"))
significanceAwayShotsTarget <- wmwTest(covid_data$AST, non_covid_data$AST, alternative = c("two.sided", "less", "greater"))
significancePercentagePointsAway <- wmwTest(covid_data$percentage_points_away, non_covid_data$percentage_points_away, alternative = c("two.sided", "less", "greater"))
significancePointsDifference <- wmwTest(covid_data$PointsDifference, non_covid_data$PointsDifference, alternative = c("two.sided", "less", "greater"))

#proportion test van thuis en uitwinsten in voor en na covid samples
prop.test(x = c(1014, 1210), n = c(1539, 2334),
          alternative = "two.sided")

#dit is voor expected goals waar veel data missing is, deze als laatste zodat je bij de andere alle data kan gebruiken
non_covid_data <- non_covid_data %>% filter(!is.na(xg1))
non_covid_data <- non_covid_data %>% filter(!is.na(xg2))
covid_data <- covid_data %>% filter(!is.na(xg1))
covid_data <- covid_data %>% filter(!is.na(xg2))

significanceExpectedGoalsHome <- wmwTest(covid_data$xg1, non_covid_data$xg1, alternative = c("two.sided", "less", "greater"))
significanceExpectedGoalsAway <- wmwTest(covid_data$xg2, non_covid_data$xg2, alternative = c("two.sided", "less", "greater"))
significanceExpectedGoalsDifference <- wmwTest(covid_data$ExpectedGoalsDifference, non_covid_data$ExpectedGoalsDifference, alternative = c("two.sided", "less", "greater"))


#dit gebruikte ik om output te kunnen kopieren, moet per test dus vrij langzaam 
output_test <- pander()
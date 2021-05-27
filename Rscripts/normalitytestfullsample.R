z1 <- ad.test(full_dataset_alan$FTHG)
z1
z2 <- ad.test(full_dataset_alan$FTAG)
z2
z3 <-ad.test(full_dataset_alan$HY) 
z3
z4 <-ad.test(full_dataset_alan$AY)
z4
z5 <-ad.test(full_dataset_alan$HR)
z5
z6 <-ad.test(full_dataset_alan$AR)
z6
z7 <-ad.test(full_dataset_alan$HF)
z7
z8 <-ad.test(full_dataset_alan$AF)
z8
z9 <-ad.test(full_dataset_alan$away_win)
z9
z10 <-ad.test(full_dataset_alan$home_win)
z10
z11 <-ad.test(full_dataset_alan$HS)
z11
z12 <-ad.test(full_dataset_alan$AS)
z12
z13 <-ad.test( full_dataset_alan$xg1)
z13
z14 <-ad.test( full_dataset_alan$xg2)
z14
z15 <- ad.test( full_dataset_alan$home_points)
z15
z16 <- ad.test(full_dataset_alan$away_points)
z16
z17 <- ad.test( full_dataset_alan$PercentagePointsHome)
z17
z18 <- ad.test( full_dataset_alan$percentage_points_away)
z18
z19 <- ad.test( full_dataset_alan$HST)
z19
z20 <- ad.test( full_dataset_alan$AST)
z20
z21 <- ad.test( full_dataset_alan$PointsDifference)
z21
z22 <- ad.test( full_dataset_alan$YellowCardDifference)
z22
z23 <- ad.test( full_dataset_alan$RedCardDifference)
z23
z24 <- ad.test( full_dataset_alan$FoulDifference)
z24
z25 <- ad.test(full_dataset_alan$GoalDifference)
result <- pander(z26)
z26 <- ad.test(full_dataset_alan$ExpectedGoalsDifference)
adlkfj <- mean(non_covid_data$PointsDifference)

englandresults<- read.csv("./England/Englandresults!.csv")
englandresults <- englandresults[ , -which(names(englandresults) %in% c("Referee"))]

englandresults2 <- read.csv("./England/Englandresults2!.csv")
englandresults2 <- englandresults2[ , -which(names(englandresults2) %in% c("Referee"))]

englandresults3<- read.csv("./England/Englandresults3!.csv")
englandresults3 <- englandresults3[ , -which(names(englandresults3) %in% c("Referee"))]


write.csv(englandresults,"C:/Documents/thesis/datasets/Englandresults!!.csv", row.names = FALSE)
write.csv(englandresults2, "C:/Documents/thesis/datasets/England/Englandresults2!!.csv", row.names = FALSE)
write.csv(englandresults3, "C:/Documents/thesis/datasets/England/Englandresults3!!.csv", row.names = FALSE)

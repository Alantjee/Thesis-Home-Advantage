install.packages('psych')
library(psych)
library(readr)
install.packages("sjPlot")
library(sjPlot)
important_statss <- cbind(full_dataset_alan[,1:2],full_dataset_alan$yel_card_spread, full_dataset_alan$age_diff,full_dataset_alan$occupancy, full_dataset_alan[,30:34], full_dataset_alan[,37:38], full_dataset_alan[,39:40],  full_dataset_alan[,52:56], full_dataset_alan[,61:62], full_dataset_alan$foul_spread)
descriptivevariables <- cbind(full_dataset_alan [, 4:5], full_dataset_alan[, 24:25], full_dataset_alan[, 30:34], full_dataset_alan[, 37:40], full_dataset_alan [, 45:48], full_dataset_alan[, 52:56], full_dataset_alan[, 61:62], full_dataset_alan[, 67:68] )
descriptivevariables <- descriptivevariables[ , -which(names(descriptivevariables) %in% c("avg_age_away", "rating_diff", "xg_diff"))]

most_important_stats <- cbind(important_stats[, 1:2], important_stats[, 8:9], important_stats[, 12:24], important_stats[, 34:35], important_stats[, 37:41], important_stats[, 46:47])
utmost_important_stats <- cbind(most_important_stats [,1:8], most_important_stats[, 11:14], most_important_stats[, 17:18], most_important_stats[, 20:26])
utmost_important_stats <- utmost_important_stats[ , -which(names(utmost_important_stats) %in% c("HF", "AF", "HY", "AY", "AR", "HR", "foreigners_home", "foreigners_away", "stadium_size" , "avg_attendance"))] 
#utmost_important_stats <- cbind(utmost_important_stats, full_dataset_alan$spi1 "spi_home", full_dataset_alan$spi2 = "spi_away", full_dataset_alan$prob1 = "prob_home_win", full_dataset_alan$prob2 = "prob_away_win", full_dataset_alan$probtie = "prob_tie", full_dataset_alan$foul_spread = "foul_spread") 
x <- describe(most_important_stats)
descrip<- stargazer(most_important_stats)
descrip <- as.data.frame(descrip)
summary(most_important_stats)
utmost_important_stats <-utmost_important_stats %>% 
  mutate_each(funs(if(is.integer(.)) as.numeric(.) else .))
columns <- colnames(utmost_important_stats)
t1 <- stargazer(
  utmost_important_stats[,columns], type = "text",digits = 2, decimals = 2,
  summary.stat = c("N", "min", "median","max", "median", "sd"))
t2 <- stargazer(most_important_stats, decimals = 2, digits = 2 , type = "text", ci=TRUE, ci.level = 0.95)
t2 <- table(t2)
print(t1)
library(here)
pdf(here("thesis", "datasets","descriptive table"))
(x)Q
dev.off()
stargazer(descriptivevariables,
            type = "text",
            title = "Descriptive Statistics",    
            out="stats_descriptives.docx")

png("summarystatistics.png", height=2000, width=3000)
p<-tableGrob(t2)
grid.arrange(p)
dev.off()
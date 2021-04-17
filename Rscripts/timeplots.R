library(ggplot2)
aggregate <- aggregate(full_dataset_alan["YellowCardDifference"], by=full_dataset_alan["quarter"], mean)
aggregate2 <- aggregate(full_dataset_alan["RedCardDifference"], by = full_dataset_alan["quarter"], mean)
aggregate3 <- aggregate(full_dataset_alan["RedCardDifference"], by = full_dataset_alan["Month_Year"], mean)
aggregate4
aggregate5
aggregate6
aggregate7
time_series_yellowcard <- ggplot(data = aggregate) + 
  geom_line(mapping = aes(x = quarter, y = YellowCardDifference))
print(time_series_yellowcard)

time_series_redcard <- ggplot(data = aggregate3) + 
  geom_line(mapping = aes(x = Month_Year, y = RedCardDifference))
print(time_series_redcard)

plot = time_series_yellowcard + 
  scale_x_date(breaks = as.Date(c("2020-03-15")))
print(plot)
str(full_dataset_alan$quarter)
full_dataset_alan$quarter <- as.Date(full_dataset_alan$quarter)
full_dataset_alan <- full_dataset_alan %>% mutate( month = floor_date(full_dataset_alan$Date, "month"))
full_dataset_alan$Month_Year <- format(as.Date(full_dataset_alan$Date), "%Y-%m")
full_dataset_alan$quarter <- quarter(full_dataset_alan$Date, with_year = TRUE)
full_dataset_alan$quarter <- as.Date(full_dataset_alan$quarter, "%q")
                                                   
library(zoo)
full_dataset_alan$quarter <- as.yearqtr(full_dataset_alan$Date, format = "%Y %q")


full_dataset_alan$aggregate <- aggregate(full_dataset_alan["HS"], by=full_dataset_alan["Month_Year"], mean)

plot(ggplot(data = aggregate) + 
       geom_smooth(mapping = aes(x = Month_Year, y = HS)))

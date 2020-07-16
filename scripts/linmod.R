


linmod <- lm(Temp ~ CO2, data=co2_temp_combined)
new <- data.frame(CO2 = only2017$CO2)
tempPredictions <- predict(linmod, newdata=new)
only2017$TempPrediction <- tempPredictions
tempActuals <- map_data[map_data$Year == 2017,]
tempActuals <- temp_actuals[,c("V2", "V5")]
only2017$TempActuals <- tempActuals$V5
only2017$Difference <- only2017$TempActuals - only2017$TempPrediction

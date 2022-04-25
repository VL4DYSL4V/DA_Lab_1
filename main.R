library(ggplot2)

airTrafficCargoStatistics <- read.csv('Air_Traffic_Cargo_Statistics.csv')
qplot(airTrafficCargoStatistics$Activity.Period, data=airTrafficCargoStatistics, geom='freqpoly')
qplot(airTrafficCargoStatistics$Cargo.Weight.LBS, data=airTrafficCargoStatistics, geom='freqpoly')
qplot(airTrafficCargoStatistics$Cargo.Metric.TONS, data=airTrafficCargoStatistics, geom='freqpoly')

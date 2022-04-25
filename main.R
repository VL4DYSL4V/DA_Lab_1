library(ggplot2)

airTrafficCargoStatistics <- read.csv('Air_Traffic_Cargo_Statistics.csv')

buildFrequencyPolygons <- function() {
  activityPeriod <- qplot(
    airTrafficCargoStatistics$Activity.Period,
    data = airTrafficCargoStatistics,
    geom = 'freqpoly'
  )
  cargoWeightLBS <- qplot(
    airTrafficCargoStatistics$Cargo.Weight.LBS,
    data = airTrafficCargoStatistics,
    geom = 'freqpoly'
  )
  cargoMetricTons <- qplot(
    airTrafficCargoStatistics$Cargo.Metric.TONS,
    data = airTrafficCargoStatistics,
    geom = 'freqpoly'
  )
  return(list(activityPeriod, cargoWeightLBS, cargoMetricTons))
}

bultWhiskersAndBoxes <- function() {
  activityPeriod <- ggplot(
    data = airTrafficCargoStatistics,
    aes(y = airTrafficCargoStatistics$Activity.Period)
  ) + geom_boxplot()
  cargoWeightLBS <- ggplot(
    data = airTrafficCargoStatistics,
    aes(y = airTrafficCargoStatistics$Cargo.Weight.LBS)
  ) +
    geom_boxplot() +
    scale_y_continuous(
      trans = log2_trans(),
      breaks = trans_breaks("log2", function(x) 2^x),
      labels = trans_format("log2", math_format(2^.x))
    )
  cargoMetricTons <- ggplot(
    data = airTrafficCargoStatistics,
    aes(y = airTrafficCargoStatistics$Cargo.Metric.TONS)
  ) +
    geom_boxplot() +
    scale_y_continuous(
      trans = log2_trans(),
      breaks = trans_breaks("log2", function(x) 2^x),
      labels = trans_format("log2", math_format(2^.x))
    )
  return(list(activityPeriod, cargoWeightLBS, cargoMetricTons))
}

buildFrequencyPolygons()
bultWhiskersAndBoxes()

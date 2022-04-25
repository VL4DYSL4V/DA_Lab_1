library(ggplot2)
library("psych")
library("DescTools")

airTrafficCargoStatistics <- read.csv('Air_Traffic_Cargo_Statistics.csv')

printDelimiter <- function () {
  cat("\n=========================================================================\n")
}

printDelimiterWithNewLines <- function () {
  cat("\n\n=========================================================================\n\n")
}

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
  return(
    list(
      activityPeriod = activityPeriod,
      cargoWeightLBS = cargoWeightLBS,
      cargoMetricTons = cargoMetricTons
    )
  )
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
  return(
    list(
      activityPeriod = activityPeriod,
      cargoWeightLBS = cargoWeightLBS,
      cargoMetricTons = cargoMetricTons
    )
  )
}

printMinMaxMeanQuartils <- function () {
  summary(
    airTrafficCargoStatistics[
      c('Activity.Period', 'Cargo.Weight.LBS', 'Cargo.Metric.TONS')
    ]
  )
}

getDeciles <- function () {
  activityPeriodDecils <- quantile(
    airTrafficCargoStatistics$Activity.Period,
    probs = seq(.1, .9, by = .1)
  )
  cargoWeightLBSDecils <- quantile(
    airTrafficCargoStatistics$Cargo.Weight.LBS,
    probs = seq(.1, .9, by = .1)
  )
  cargoMetricWeightTonsDecils <- quantile(
    airTrafficCargoStatistics$Cargo.Metric.TONS,
    probs = seq(.1, .9, by = .1)
  )
  return(
    list(
      activityPeriodDecils = activityPeriodDecils,
      cargoWeightLBSDecils = cargoWeightLBSDecils,
      cargoMetricWeightTonsDecils = cargoMetricWeightTonsDecils
    )
  )
}

printGeometricalMeanWithoutZeroes <- function (vector, name) {
  print(
    paste(
      "Geometric Mean of '", name, "': ", exp(mean(log(vector[vector>0])))
    )
  )
}

printHarmonicMeanWithoutZeroes <- function (vector, name) {
  print(
    paste(
      "Harmonic Mean of '", name, "': ", harmonic.mean(vector, zero = FALSE)
    )
  )
}

printMode <- function (vector, name) {
  vectorMode <- Mode(vector)
  print(
    paste(
      "Mode of '", name, "': ", toString(vectorMode)
    )
  )
}

buildFrequencyPolygons()
bultWhiskersAndBoxes()

printMinMaxMeanQuartils()
printDelimiterWithNewLines()

decils <- getDeciles()
print("'Activity Period' Decils:")
print(decils$activityPeriodDecils)
print("'Cargo Weight LBS' Decils:")
print(decils$cargoWeightLBSDecils)
print("'Cargo Weight Tons' Decils:")
print(decils$cargoMetricWeightTonsDecils)
printDelimiterWithNewLines()

printGeometricalMeanWithoutZeroes(airTrafficCargoStatistics$Activity.Period, "Activity Period")
printHarmonicMeanWithoutZeroes(airTrafficCargoStatistics$Activity.Period, "Activity Period")
printMode(airTrafficCargoStatistics$Activity.Period, "Activity Period")
printDelimiter()

printGeometricalMeanWithoutZeroes(airTrafficCargoStatistics$Cargo.Weight.LBS, "Cargo Weight LBS")
printHarmonicMeanWithoutZeroes(airTrafficCargoStatistics$Cargo.Weight.LBS, "Cargo Weight LBS")
printMode(airTrafficCargoStatistics$Cargo.Weight.LBS, "Cargo Weight LBS")
printDelimiter()

printGeometricalMeanWithoutZeroes(airTrafficCargoStatistics$Cargo.Metric.TONS, "Cargo Metric Tons")
printHarmonicMeanWithoutZeroes(airTrafficCargoStatistics$Cargo.Metric.TONS, "Cargo Metric Tons")
printMode(airTrafficCargoStatistics$Cargo.Metric.TONS, "Cargo Metric Tons")
printDelimiter()

library(ggplot2)
library("psych")
library("DescTools")
library("e1071")

airTrafficCargoStatistics <- read.csv('Air_Traffic_Cargo_Statistics.csv')

printDelimiterWithNewLines <- function () {
  cat("\n\n=========================================================================\n\n")
}

printEmptyLine <- function () {
  cat("\n")
}

buildFrequencyPolygons <- function() {
  activityPeriod <- qplot(
    Activity.Period,
    data = airTrafficCargoStatistics,
    geom = 'freqpoly'
  )
  cargoWeightLBS <- qplot(
    Cargo.Weight.LBS,
    data = airTrafficCargoStatistics,
    geom = 'freqpoly'
  )
  cargoMetricTons <- qplot(
    Cargo.Metric.TONS,
    data = airTrafficCargoStatistics,
    geom = 'freqpoly',
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
    aes(y = Activity.Period)
  ) + geom_boxplot()
  cargoWeightLBS <- ggplot(
    data = airTrafficCargoStatistics,
    aes(y =Cargo.Weight.LBS)
  ) +
    geom_boxplot() +
    scale_y_continuous(
      trans = log2_trans(),
      breaks = trans_breaks("log2", function(x) 2^x),
      labels = trans_format("log2", math_format(2^.x))
    )
  cargoMetricTons <- ggplot(
    data = airTrafficCargoStatistics,
    aes(y = Cargo.Metric.TONS)
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

printSummary <- function (vector, name) {
  summary <- summary(vector)
  print(paste("Summary of '", name, "': "))
  print(summary)
}

printDeciles <- function (vector, name) {
  deciles <- quantile(
    vector,
    probs = seq(.1, .9, by = .1)
  )
  print(paste("Deciles of '", name, "': "))
  print(deciles)
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

printDispersion <- function (vector, name) {
  dispersion <- var(vector)
  print(
    paste(
      "Dispersion of '", name, "': ", dispersion
    )
  )
}

printStandardDeviation <- function (vector, name) {
  sd <- sd(vector)
  print(
    paste(
      "Standard Deviation of '", name, "': ", sd
    )
  )
}

printCoefficientOfVariation <- function (vector, name) {
  cv <- sd(vector) / mean(vector) * 100
  print(
    paste(
      "Coefficient of Variation of '", name, "': ", cv
    )
  )
}

printProbabilisticDeviation <- function (vector, name) {
  pd <- IQR(vector) / 2
  print(
    paste(
      "Probabilistic Deviation of '", name, "': ", pd
    )
  )
}

printSamplingSpan <- function (vector, name) {
  max <- max(vector)
  min <- min(vector)
  print(
    paste(
      "Sampling Span of '", name, "': ", max - min
    )
  )
}

printConcentrationInterval <- function (vector, name) {
  mean <- mean(vector)
  sd <- sd(vector)
  print(
    paste(
      "Concentration Interval of '", name, "': (", mean - 3 * sd, ", ", mean + 3 * sd, ")"
    )
  )
}

printKurtosis <- function (vector, name) {
  print(
    paste(
      "Kurtosis of '", name, "': ", kurtosis(vector)
    )
  )
}

printSkewness <- function (vector, name) {
  print(
    paste(
      "Skewness of '", name, "': ", skewness(vector)
    )
  )
}

buildFrequencyPolygons()
bultWhiskersAndBoxes()

printDelimiterWithNewLines()

analyze <- function (vector, vectorName) {
  printSummary(vector, vectorName)
  printEmptyLine()
  printDeciles(vector, vectorName)
  printEmptyLine()
  printGeometricalMeanWithoutZeroes(vector, vectorName)
  printEmptyLine()
  printHarmonicMeanWithoutZeroes(vector, vectorName)
  printEmptyLine()
  printMode(vector, vectorName)
  printEmptyLine()
  printDispersion(vector, vectorName)
  printEmptyLine()
  printStandardDeviation(vector, vectorName)
  printEmptyLine()
  printCoefficientOfVariation(vector, vectorName)
  printEmptyLine()
  printProbabilisticDeviation(vector, vectorName)
  printEmptyLine()
  printSamplingSpan(vector, vectorName)
  printEmptyLine()
  printConcentrationInterval(vector, vectorName)
  printEmptyLine()
  printKurtosis(vector, vectorName)
  printEmptyLine()
  printSkewness(vector, vectorName)
  printDelimiterWithNewLines()
}

analyze(airTrafficCargoStatistics$Activity.Period, "Activity Period")
analyze(airTrafficCargoStatistics$Cargo.Weight.LBS, "Cargo Weight LBS")
analyze(airTrafficCargoStatistics$Cargo.Metric.TONS, "Cargo Metric Tons")

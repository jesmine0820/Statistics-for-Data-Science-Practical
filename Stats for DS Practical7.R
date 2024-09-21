# Load necessary libraries
library(forecast)
library(tseries)
library(lmtest)

#------------------------ Nonseasonal Example (Example 7.3.1: Internet Users) ------------------------

# Data for non-seasonal example
Y <- c(88, 84, 85, 85, 84, 85, 83, 85, 88, 89, 91, 99, 104, 112,
       126, 138, 146, 151, 150, 148, 147, 149, 143, 132, 131,
       139, 147, 150, 148, 145, 140, 134, 131, 131, 129, 126,
       126, 132, 137, 140, 142, 150, 159, 167, 170, 171, 172,
       172, 174, 175, 172, 172, 174, 174, 169, 165, 156, 142,
       131, 121, 112, 104, 102, 99, 99, 95, 88, 84, 84, 87, 89,
       88, 85, 86, 89, 91, 91, 94, 101, 110, 121, 135, 145, 149,
       156, 165, 171, 175, 177, 182, 193, 204, 208, 210, 215,
       222, 228, 226, 222, 220)
Y <- ts(Y)

# Plot the time series and ACF/PACF
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(Y, ylab="Number of Users", main="Number of Internet Users")
acf(Y)
pacf(Y)

# First differencing
z <- ts(diff(Y, lag=1))
ts.plot(z, gpars=list(main= "First Differences", xlab="Week", ylab="Number of Users"))
acf(z, main="ACF of First Differences")
pacf(z, main="PACF of First Differences")
adf.test(z)

# Second differencing (if needed)
z2 <- ts(diff(z, lag=1))
ts.plot(z2, gpars=list(main= "Second Differences", xlab="Week", ylab="Number of Users"))
acf(z2, main="ACF of Second Differences")
pacf(z2, main="PACF of Second Differences")
adf.test(z2)

# Fit ARIMA models
fit1 <- arima(Y, order=c(3,1,0)); fit1
checkresiduals(fit1, lag=24)
plot(forecast(fit1, h=12))

fit2 <- arima(Y, order=c(2,2,0)); fit2
checkresiduals(fit2, lag=24)
plot(forecast(fit2, h=12))

fit3 <- arima(Y, order=c(0,2,2)); fit3
checkresiduals(fit3, lag=24)
plot(forecast(fit3, h=12))

fit4 <- auto.arima(Y, ic="aic", trace=TRUE); fit4
checkresiduals(fit4, lag=24)
plot(forecast(fit4, h=12))

# AIC matrix for model selection
AIC <- matrix(0, 6, 6)
for (p in 0:5) {
  for (q in 0:5) {
    mod.fit <- arima(Y, order=c(p,1,q))
    AIC[p+1, q+1] <- mod.fit$aic
  }
}
AIC

#------------------------- Seasonal Example (Example 7.3.1: Writing Dataset) -------------------------

# Seasonal data
Y <- c(562.674, 599.000, 668.516, 597.798, 579.889, 668.233, 499.232, 215.187,
       555.813, 586.935, 546.136, 571.111, 634.712, 639.283, 712.182, 621.557,
       621.000, 675.989, 501.322, 220.286, 560.727, 602.530, 626.379, 605.508,
       646.783, 658.442, 712.906, 687.714, 723.916, 707.183, 629.000, 237.530,
       613.296, 730.444, 734.925, 651.812, 676.155, 748.183, 810.681, 729.363,
       701.108, 790.079, 594.621, 230.716, 617.189, 691.389, 701.067, 705.777,
       747.636, 773.392, 813.788, 766.713, 728.875, 749.197, 680.954, 241.424,
       680.234, 708.326, 694.238, 772.071, 795.337, 788.421, 889.968, 797.393,
       751.000, 821.255, 691.605, 290.655, 727.147, 868.355, 812.390, 799.556,
       843.038, 847.000, 941.952, 804.309, 840.307, 871.528, 656.330, 370.508,
       742.000, 847.152, 731.675, 898.527, 778.139, 856.075, 938.833, 813.023,
       783.417, 828.110, 657.311, 310.032, 780.000, 860.000, 780.000, 807.993,
       895.217, 856.075, 893.268, 875.000, 835.088, 934.595, 832.500, 300.000,
       791.443, 900.000, 781.729, 880.000, 875.024, 992.968, 976.804, 968.697,
       871.675, 1006.852, 832.037, 345.587, 849.528, 913.871, 868.746, 993.733)

Y <- ts(Y, frequency=12)

# Plot and seasonal differencing
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(Y)
acf(Y, lag.max=40)
pacf(Y, lag.max=40)

ndiffs(Y)
nsdiffs(Y)

z <- ts(diff(Y, lag=12))
ts.plot(z, gpars=list(main="First (Seasonal) Differences", xlab="Week", ylab="Sales"))
acf(z, main="ACF of First Seasonal Differences", lag.max=40)
pacf(z, main="PACF of First Seasonal Differences", lag.max=40)
adf.test(z)

z2 <- ts(diff(z, lag=1))
ts.plot(z2, gpars=list(main="Second (Nonseasonal) Differences", xlab="Week", ylab="Sales"))
acf(z2, main="ACF of Second Differences", lag.max=40)
pacf(z2, main="PACF of Second Differences", lag.max=40)
adf.test(z2)

# Fit ARIMA models
fit1 <- arima(Y, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12)); summary(fit1)
checkresiduals(fit1, lag=24)
plot(forecast(fit1, h=12))

fit2 <- auto.arima(Y, ic="aic", trace=TRUE); summary(fit2)
checkresiduals(fit2, lag=24)
plot(forecast(fit2, h=12))

# Test the significance of coefficients
coeftest(fit1)

#------------------------- AirPassengers Example (ARIMA) -------------------------

AirPassengers <- read.csv("AirPassengers.csv")
Y <- ts(AirPassengers[,2], frequency=12, start=c(1949,1))

# Split into train and test sets
train <- window(Y, start=1949, end=c(1956,12))
test <- window(Y, start=1957, end=c(1960,12))

# Plot the data
plot(Y, ylab="Passengers", main="Number of Air Passengers from January 1949 to December 1960")

# Check for differencing
ndiffs(train)
nsdiffs(train)

# Seasonal and non-seasonal differencing
SDiff <- ts(diff(train, lag=12))
Diff <- ts(diff(SDiff, lag=1))

# ARIMA model fitting
fit <- arima(train, order=c(1,1,0), seasonal=list(order=c(1,1,0), period=12))
summary(fit)
checkresiduals(fit)

# Forecasting
fr_air <- forecast(fit, h=12 * 4)  # Forecast for the next 4 years
layout(matrix(c(1, 1)))
plot(fr_air, main="Forecast of Air Passengers")
lines(test, col="turquoise2")
accuracy(fr_air, test)

# Auto ARIMA for comparison
fit_auto_air <- auto.arima(train, ic="aic", trace=TRUE)
summary(fit_auto_air)
checkresiduals(fit_auto_air)

# Forecasting using Auto ARIMA
fr_auto_air <- forecast(fit_auto_air, h=12 * 4)
layout(matrix(c(1, 1)))
plot(fr_auto_air, main="Auto ARIMA Forecast of Air Passengers")
lines(test, col="turquoise2")
accuracy(fr_auto_air, test)
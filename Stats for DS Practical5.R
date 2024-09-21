# Load necessary libraries
library(forecast)

# Load and prepare data
AirPassengers <- read.csv("AirPassengers.csv")
Data <- ts(AirPassengers[, 2], frequency = 12, start = c(1949, 1))

# Plot the time series data
plot(Data, ylab = "Passengers", main = "Number of Air Passengers from January 1949 to December 1960", xlab = "Year")

# Step 1: Split Data into Training and Testing
train <- window(Data, start = c(1949, 1), end = c(1955, 12))
test <- window(Data, start = c(1956, 1), end = c(1960, 12))

# Section 1: Linear Model - Extrapolation of Trend Curves
fit <- tslm(train ~ time(train))  # Linear trend model
summary(fit)  # Summary of the model

# Check residuals of the fit model
checkresiduals(fit$residuals)

# Forecast based on linear model
forecast <- forecast(fit, newdata = data.frame(train = time(test)))
forecast  # View forecast output

# Plot forecast
plot(forecast, main = "Linear Model Forecast vs Test Data")

# Accuracy of forecast
accuracy(forecast, test)

# Section 2: Multiplicative Decomposition
components <- decompose(train, type = "multiplicative")  # Perform multiplicative decomposition
trend <- components$trend  # Extract trend
seasonal <- components$seasonal  # Extract seasonal component
remainder <- components$random  # Extract remainder

# Plot components of decomposition (no "main" argument)
plot(components)  # Plot decomposition
title(main = "Multiplicative Decomposition of Train Data")

# Check residuals of the remainder
checkresiduals(remainder)

# Section 3: Forecast from STL + Random Walk (STLF Method)
fcast <- stlf(train, method = 'naive', h = length(test))  # STL decomposition and forecasting

# Summary of forecast
summary(fcast)

# Check residuals of STL forecast
checkresiduals(fcast)

# Plot STL forecast
plot(fcast, main = "STL + Naive Forecast vs Test Data")

# Accuracy of STL forecast
accuracy(fcast, test)

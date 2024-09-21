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

# Section 1: Simple Exponential Smoothing (Weighted Moving Average)
es1 <- HoltWinters(train, alpha = 0.1, beta = FALSE, gamma = FALSE)
es1

# Check residuals
checkresiduals(es1)

# Forecast for next 4 years (12*4 months)
f1 <- forecast(es1, h = 12*4)

# Compare accuracy
accuracy(f1, test)

# Plot forecast with confidence intervals
forecast <- predict(es1, n.ahead = 12*4, prediction.interval = TRUE, level = 0.95)
plot(es1, forecast, ylim = c(100, 620), main = "Simple Exponential Smoothing Forecast")
lines(test, col = "turquoise2")

# Section 2: Additive Holt-Winters Method
hw_additive <- HoltWinters(train, alpha = 0.2, beta = 0.1, gamma = 0.1, seasonal = "additive")
hw_additive

# Check residuals
checkresiduals(hw_additive)

# Forecast using Additive Holt-Winters
f2 <- forecast(hw_additive, h = 12*4)
accuracy(f2, test)

# Plot forecast
forecast <- predict(object = hw_additive, n.ahead = 12*4, prediction.interval = TRUE, level = 0.95)
plot(hw_additive, forecast, ylim = c(100, 620), main = "Additive Holt-Winters Forecast")
lines(test, col = "turquoise2")

# Section 3: Multiplicative Holt-Winters Method
hw_multiplicative <- HoltWinters(train, alpha = 0.2, beta = 0.1, gamma = 0.1, seasonal = "mult")
hw_multiplicative

# Check residuals
checkresiduals(hw_multiplicative)

# Forecast using Multiplicative Holt-Winters
f3 <- forecast(hw_multiplicative, h = 12*4)
accuracy(f3, test)

# Plot forecast
forecast <- predict(object = hw_multiplicative, n.ahead = 12*4, prediction.interval = TRUE, level = 0.95)
plot(hw_multiplicative, forecast, ylim = c(100, 620), main = "Multiplicative Holt-Winters Forecast")
lines(test, col = "turquoise2")

# Section 4: Holt-Winters Method without specifying smoothing parameters (Default: Additive)
hw_default <- HoltWinters(train)
hw_default

# Check residuals
checkresiduals(hw_default)

# Forecast using default Holt-Winters
f4 <- forecast(hw_default, h = 12*4)
accuracy(f4, test)

# Plot forecast
forecast <- predict(object = hw_default, n.ahead = 12*4, prediction.interval = TRUE, level = 0.95)
plot(hw_default, forecast, ylim = c(100, 620), main = "Default Holt-Winters (Additive) Forecast")
lines(test, col = "turquoise2")

# Section 5: Holt-Winters Method with Multiplicative Seasonality
hw_mult_seasonal <- HoltWinters(train, seasonal = "mult")
hw_mult_seasonal

# Check residuals
checkresiduals(hw_mult_seasonal)

# Forecast using Multiplicative Seasonality Holt-Winters
f5 <- forecast(hw_mult_seasonal, h = 12*4)
accuracy(f5, test)

# Plot forecast
forecast <- predict(object = hw_mult_seasonal, n.ahead = 12*4, prediction.interval = TRUE, level = 0.95)
plot(hw_mult_seasonal, forecast, ylim = c(100, 620), main = "Multiplicative Seasonality Holt-Winters Forecast")
lines(test, col = "turquoise2")

# Section 6: ETS Model (Exponential Smoothing State Space Model)
fit_ets <- ets(train)
summary(fit_ets)

# Check residuals
checkresiduals(fit_ets)

# Forecast using ETS model
f_ets <- forecast(fit_ets, h = 12*4)
accuracy(f_ets, test)

# Plot forecast
plot(f_ets, main = "ETS Model Forecast")
lines(test, col = "turquoise2")

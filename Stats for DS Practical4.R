# Load the CSV data
AirPassengers <- read.csv("AirPassengers.csv")

# Convert the second column (passenger numbers) into a time series object
Data <- ts(AirPassengers[,2], frequency = 12, start = c(1949, 1))

# Plot the time series data
plot(Data, ylab = "Passengers", main = "Number of Air Passengers from January 1949 to December 1960", xlab = "Year")

## Step 0: Split univariate time series data into training and testing

## Alternative 1: By year
# Use train data from 1949 to 1955 for forecasting
train <- window(Data, start = c(1949, 1), end = c(1955, 12))
train

# Use remaining data from 1956 to 1960 to test accuracy
test <- window(Data, start = c(1956, 1), end = c(1960, 12))
test

## Alternative 2: By percentage
library(forecast)
train <- head(Data, round(length(Data) * 0.67))  # 67% for training
h <- length(Data) - length(train)  # Remaining for testing
test <- tail(Data, h)

# Plot training and testing data
autoplot(train) + autolayer(test)

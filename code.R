rm(list=ls())

library(lubridate)
library(dplyr)
library(xts)
library(ggfortify)
library(ggplot2)
library(forecast)
library(stats)

nifty50=read.csv("D:/Project/Time Series/NIFTY 50_Data.csv")
nifty50$Date = dmy(nifty50$Date)

nifty50 = nifty50 %>% 
  arrange(Date) %>%
  tibble::rownames_to_column(var = "row_id") %>%
  select(-row_id) %>%
  as.xts(order.by = nifty50$Date)

nifty50_monthly = aggregate(nifty50$Close, as.yearmon, last)

nifty50.ts = ts(nifty50_monthly, start = start(nifty50_monthly), frequency = 12)

# Plotting Data and Trend Line
autoplot(nifty50.ts) +
  ggtitle("Nifty Monthly Closing Points") +
  xlab("Year") +
  ylab( "Monthly Closing")

# Plotting trend line
plot(nifty50.ts)
abline(reg = lm(nifty50.ts~time(nifty50.ts)))

# Analyzing Seasonal Component
ggmonthplot(nifty50.ts) +
  ggtitle("Monthly Nifty Chart")

# Checking Seasonal Pattern
ggseasonplot(nifty50.ts) +
  ggtitle("seasonal Pattern")

# Decomposition
nifty50.ts.decomposition = stl(as.numeric(nifty50.ts))
autoplot(nifty50.ts.decomposition,main="Decomposition by st1 method")

# Simple Exponential Smoothing
nifty50ses = ses(nifty50.ts)
autoplot(nifty50ses)
nifty50ses$model
summary(nifty50ses)

# Double Exponential Smoothing
nifty50holt = holt(nifty50.ts,h=24)
autoplot(nifty50holt)
nifty50holt$model
summary(nifty50holt)

# Triple Exponential Smoothing
nifty50hw = hw(nifty50.ts,h=24)
autoplot(nifty50hw)
nifty50hwmodel
summary(nifty50hw)

# Visual Test for Stationarity
hist(nifty50.ts)

# Lap Plots - each lag corresponds to correlation coefficient
lags = window(nifty50.ts)
gglagplot(lags)

# ACF of original time series
ggAcf(lags)

# Augmented Dickey-Fuller Test (I)
## Null Hypothesis H0: Time Series non-stationary
## Alternative hypothesis H1: Time series is stationary
adf.test(nifty50.ts)

# Differencing
## Remove the unequal variance and remove the trend component
nifty50_d1 = diff(nifty50.ts,differences = 1)
autoplot(nifty50_d1, main = "First Order Differencing")

# Augmented Dickey-Fuller Test (I)
## Null Hypothesis H0: Time Series non-stationary
## Alternative hypothesis H1: Time series is stationary
adf.test(nifty50_d1,alternative = "stationary", k=12)

# Creating Training and Test Dataset
# Assuming you have a time series object named "nifty50.ts"

tsdatatrain = window(nifty50.ts, end=c(2020,12), frequency = 12) # Train Data Set
tsdatatest = window(nifty50.ts, start=c(2021,1), frequncy = 12) # Test Data Test

# Auto Arima Model Nifty 50
autoarima.fit.train = auto.arima(tsdatatrain, seasonal = TRUE, ic="bic")
autoarima.fit.train

# Checking Accuracy of the Model
accuracy(autoarima.fit.train.forecast, tsdatatest)

# Checking Residuals
checkresiduals(autoarima.fit.train.forecast)

# Fitting Arima Model
fit = Arima(tsdatatrain, c(0,1,0), seasonal = list(order=c(0,0,2),period = 12))
plot(fit$x,col="blue")
lines(fit$fitted,col="red", main="Nifty50 Actual vs Forecast")

# Forecasting using ARIMA
forecast(fit, h=12)


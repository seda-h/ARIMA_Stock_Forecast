# This is a attempt to forecast Stock Prices by using ARIMA Models
# For more information:
# https://www.youtube.com/watch?v=qaZNDKFnX_Y
# DataCamp.com - Quantitative Analyst Track
# David Stoffer - Time Series Analysis and Its Applications: With R Examples


# Library - Loading the required libraries
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(astsa)

# If packages are missing use install.packages("") and update them if necessary
# update.packages(repos='http://cran.rstudio.com/', ask=FALSE, checkBuilt=TRUE)
# install.packages("quantmod")
# install.packages("tseries")
# install.packages("timeSeries")
# install.packages("forecast")
# install.packages("xts")
# install.packages("astsa")

# Pull Data from Yahoo Finance
getSymbols("BABA", from = "2010-01-01", to = "2020-10-31", src =  "yahoo", adjust =  TRUE)

# Use closed prices for each trading day (4th column)
Stock_Data = BABA[,4]

# Plot data - check the data for the last days/months/years
plot(Stock_Data, main = "Stock Prices - Closed")

# Compute and plot the log returns for the stock to see the approximate growth rate
stock = diff(log(Stock_Data))
stock = stock[!is.na(stock)]
plot(stock,type='l', main='log returns plot')

# Graph the ACF and PACF to identify lags

# In the ACF plot, the x-axis expresses the correlation coefficient 
# whereas the y-axis mentions the number of lags.
# PACF can be explained using a linear regression

#           AR(p)                 MA(q)                       ARMA(p,q)
# ACF       Tails off             Cuts off after lag q        Tails off
# PACF      Cuts of after lag p   Tails off                   Tails off
acf2(diff(Stock_Data))

# Auto Arima to determine the AR(p) I MA(q) values (p,d,q)
# AIC & BIC should be as low as possible
# Find a model that has the smallest AIC/BIC
auto.arima(Stock_Data, seasonal = FALSE) # 0,1,3
fitA = auto.arima(Stock_Data, seasonal = FALSE)
tsdisplay(residuals(fitA), main='Auto Arima Model Residuals')

# Default Model (1,1,1)
fitB = arima(Stock_Data, order=c(1,1,1))
tsdisplay(residuals(fitB), main='(1,1,1) Model Residuals')

# Test own Model and input own (p,d,q)
fitC = arima(Stock_Data, order=c(2,1,2))
tsdisplay(residuals(fitC), lag.max=100, main='(2,1,2) Model Residuals')

fitD = arima(Stock_Data, order=c(2,2,2))
tsdisplay(residuals(fitD), lag.max=100, main='(2,2,2) Model Residuals')

# Plot the ARIMA-Models above & forecast 90 days
par(mfrow=c(2,2))
term<-90 # forecast for x days
fcast1 <- forecast(fitA, h=term)
plot(fcast1)
fcast2 <- forecast(fitB, h=term)
plot(fcast2)
fcast3 <- forecast(fitC, h=term)
plot(fcast3)
fcast4 <- forecast(fitD, h=term)
plot(fcast4)

# Calculate the accuracy of the models above by subtracting MAPE from 100.
accuracy(fcast1)
accuracy(fcast2) 
accuracy(fcast3) 
accuracy(fcast4)

# Use sarima
auto.arima(Stock_Data)
sarima(Stock_Data, 3,1,3) # Input from auto.arima
sarima.for(Stock_Data, n.ahead=90, p=3,d=1,q=3)






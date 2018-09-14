library(xlsx)
library(tidyr)
library(Metrics)
library(smooth)
library(TTR)
library(forecast)
library(tseries)

setwd('F:\\ISS KE\\Semester 2\\developing intelligent systems for performing business analytics\\Lecture Notes\\Rita - Day 5 Material')
getwd()
data <- read.xlsx("ActualRatings_weeklyGRP.xls",sheetIndex = 2)
data <- separate(data = data , col = GRPRatingsDate, into = c("Date", "wk_num"), sep = 12)
data <- data[,-2]
t_data <- ts (data$GRP, start = c(2007,25), frequency = 52) # frequency 4 => Quarterly Data
plot(t_data)
# splitting data into train and test as per the CA requirements
train_ts <- window(t_data,start=c(2007,25),end=c(2008,44))
plot(train_ts)
test_ts <- window(t_data,start=c(2008,45),end=c(2009,12))
plot(test_ts)

# Linear regression
# creating a dataframe from the TS object to perform linear regresion
data_1 <- data
data_1$num <- c(1:length(data$GRP))
data_1$Date <- c()
data_1$num2 <- data_1$num^2
data_1$num3 <- data_1$num^3

# splitting data into train and test as per the CA requirements
train_dat <- data_1[1:72,]
test_dat <- data_1[73:length(data_1$GRP),]
# Applying linear regression
lm_model <- lm(GRP ~ num, train_dat)
summary(lm_model)
test_pred <- predict(lm_model, test_dat)
lm_test_ts <- ts(test_pred,start=c(2008,45),frequency = 52)
# Metrics and plot of the time series regression output.
rmse(test_dat$GRP,test_pred)
MAPE(test_dat$GRP,test_pred)
mse(test_dat$GRP,test_pred)
mad(test_dat$GRP,test_pred)
ts.plot(t_data,lm_test_ts,col=c("blue","red"), type="o", main = "Time Series Regression", sub = "Original vs Predicted", ylab = "Ratings")
legend("topright", legend = c("Original","Predicted"), col = c("blue","red"), lty = 1)

# simple exponential smoothing
es_s_model <- HoltWinters(train_ts,beta = FALSE, gamma = FALSE)
es_s_model
es_s_pred <- predict(es_s_model, 20, prediction.interval = FALSE)
ts.plot(t_data,es_s_pred,col=c("red","blue"))
rmse(test_dat$GRP,es_s_pred)
MAPE(test_dat$GRP,es_s_pred)

# double exponential smoothing
es_d_model <- HoltWinters(train_ts, gamma = FALSE)
es_d_model
es_d_pred <- predict(es_d_model, 20, prediction.interval = FALSE)
ts.plot(t_data,es_d_pred,col=c("red","blue"))
rmse(test_dat$GRP,es_d_pred)
MAPE(test_dat$GRP,es_d_pred)

# triple exponential smoothing - no seasonality - not required

#ARIMA

plot(train_ts)
adf.test(train_ts, alternative = "stationary")
train_ts1 = diff(train_ts, differences = 1)
plot(train_ts1)
adf.test(train_ts1, alternative = "stationary")

Acf(train_ts1)
Pacf(train_ts1)

auto.arima(train_ts, seasonal = FALSE)
fit1 = arima(train_ts, order = c(1,1,1))
fcast <- forecast(fit1, h = 20)
plot.ts(t_data,col=c("red","blue"))
ts.plot(t_data,fcast$mean,col=c("red","blue"))
rmse(test_dat$GRP, fcast$mean)
MAPE(test_dat$GRP, fcast$mean)




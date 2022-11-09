rm(list=ls(all=TRUE))

library(dplyr)
library(lubridate)
library(forecast)
library(tidyr)
library(zoo)
library(imputeTS)

#loading in dataset#
data<-read.csv("coffee.csv")

## Data cleansing ##

#Change it into date format
data$Date <- as.Date(data$Date)

#Complete the data and filled with average mean for the NA
data <- data %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
  na_interpolation(option = "linear")

data <- data %>% fill(Currency)

#Creating time series - whole data set with different frequencies
daily<- ts(data$High, start = c(1), frequency = 1)
plot(daily)

weekly<- ts(data$High, start = c(1), frequency = 7)
plot(weekly)
autoplot(decompose(weekly, type = "additive"))

yearly <- ts(data$High, start = c(1), frequency = 365)
autoplot(yearly)
autoplot(decompose(yearly, type = "additive"))
acf(yearly)
pacf(yearly)

##creating time series for post peak
weekly.peak <- ts(window(data$High, start=c(4800)), frequency=7)
plot(weekly.peak)
autoplot(decompose(weekly.peak, type = "additive"))
acf(weekly.peak)
pacf(weekly.peak)

#Creating train & validation set time series for the whole data set
nValid.w <- length(weekly) * 0.25
nTrain.w <- length(weekly) - nValid.w

train.ts.w <- window(weekly, start = c(1,1), end = c(1,length(weekly) - nValid.w))
valid.ts.w <- window(weekly, start = c(1,length(weekly) - nValid.w + 1))
plot(train.ts.w, type = "l", xlab = "Time", ylab = "Price", xlim=c(0,1300))
lines(valid.ts.w, col = "green")

#Creating train & validation set time series from post peak
nValid.2011 <- (length(weekly) - 4800) * 0.25
nTrain.2011 <- (length(weekly) - 4800) - nValid.2011

train.ts.2011 <- window(weekly, start = c(1,length(weekly) - nTrain.2011 - nValid.2011), end = c(1,length(weekly) - nValid.2011))
valid.ts.2011 <- window(weekly, start = c(1,length(weekly) - nValid.2011 + 1))
plot(train.ts.2011, type = "l", xlab = "Time", ylab = "Price", xlim=c(680,1200), ylim=c(80,270))
lines(valid.ts.2011, col = "green")

#Creating train & validation set for the latest year
nValid <- 13*7
nTrain <- 39*7

train.ts <- window(weekly, start = c(1,length(weekly) - nTrain - nValid), end = c(1,length(weekly) - nValid))
valid.ts <- window(weekly, start = c(1,length(weekly) - nValid + 1))
plot(train.ts, xlim = c(1130,1200), type = "l", xlab = "Time", ylab = "Price")
lines(valid.ts, col = "green")

#Decompose the last year
window.ts <- window(weekly, start = c(1,length(weekly) - nTrain - nValid), end = c(1,length(weekly)))
plot(decompose(window.ts))
pacf(window.ts, lag=13*7)
acf(window.ts, lag=13*7)

#Mean prediction
aver.pred <- mean(train.ts)
aver.pred.forecast <- forecast(aver.pred, h = nValid)
plot(aver.pred.forecast)
accuracy(aver.pred.forecast, valid.ts)

#Rolling mean
rollmean.pred <- rollmean(train.ts, k = 7, align = "right")
rollmean.pred.forecast <- forecast(rollmean.pred, h = nValid)
plot(rollmean.pred.forecast)
lines(valid.ts, col="green")
accuracy(rollmean.pred.forecast, valid.ts)

#Linear model with trend
train.lm.trend <- tslm(train.ts ~ trend)
train.lm.trend.forecast <- forecast(train.lm.trend, h = nValid)
plot(train.lm.trend.forecast)
lines(valid.ts, col="green")
accuracy(train.lm.trend.forecast, valid.ts)

#Linear model with season
train.lm.season <- tslm(train.ts ~ season)
train.lm.season.forecast <- forecast(train.lm.season, h = nValid)
plot(train.lm.season.forecast)
lines(valid.ts, col="green")
accuracy(train.lm.season.forecast, valid.ts)

#Linear model with trend + season
train.lm.trend.season <- tslm(train.ts ~ trend + season)
train.lm.trend.season.forecast <- forecast(train.lm.trend.season, h = nValid)
plot(train.lm.trend.season.forecast)
lines(valid.ts, col="green")
accuracy(train.lm.trend.season.forecast, valid.ts)

#Seasonal naive
snaive.pred <- snaive(train.ts, h = nValid)
plot(snaive.pred)
accuracy(snaive.pred, valid.ts)
lines(valid.ts, col="green")

#Holt-winter
train.hw <- stlf(train.ts, method=c("ets"), etsmodel = "ZZZ", h= nValid)
hw.pred <- forecast(train.hw, h = nValid)
plot(hw.pred)
lines(valid.ts, col="green")
accuracy(hw.pred,valid.ts)

#arima model
test_arima <- auto.arima(train.ts)
checkresiduals(test_arima)
test_arima %>% forecast(h=13*7)
plot(test_arima %>% forecast(h=13*7))
lines(valid.ts, col="green")
accuracy(test_arima %>% forecast(h=13*7), valid.ts)

##snaive model for whole data set
snaive.w.pred <- snaive(train.ts.w, h = nValid.w)
plot(snaive.w.pred)
accuracy(snaive.w.pred, valid.ts.w)
lines(valid.ts.w, col="green")

##tslm model for whole data set
train.lm.trend.season.w <- tslm(train.ts.w ~ trend + season)
train.lm.trend.season.w.forecast <- forecast(train.lm.trend.season.w, h = nValid.w)
plot(train.lm.trend.season.w.forecast)
lines(valid.ts.w, col="green")
accuracy(train.lm.trend.season.w.forecast, valid.ts.w)

##snaive model for 2011 onwards
snaive.2011.pred <- snaive(train.ts.2011, h = nValid.2011)
plot(snaive.2011.pred)
accuracy(snaive.w.pred, valid.ts.2011)
lines(valid.ts.2011, col="green")

##tslm model for 2011 onwards
train.lm.trend.season.2011 <- tslm(train.ts.2011 ~ trend + season)
train.lm.trend.season.2011.forecast <- forecast(train.lm.trend.season.2011, h = nValid.2011)
plot(train.lm.trend.season.2011.forecast)
lines(valid.ts.2011, col="green")
accuracy(train.lm.trend.season.2011.forecast, valid.ts.2011)

#tslm model with trend and seasonality on validation set
plot(train.lm.trend.season.forecast,
     ylim = c(160,300 ),
     ylab = "Daily Price High",
     xlab = "Week",
     xlim = c(1131,1183),
     main = "Predictive Model for 3 months using Linear model with Trend and Sesonality")
lines(valid.ts, col="green")
text(1151, 290, "Training")
text(1176, 290, "Validation")

#applying tslm(trend + season) model to 52 weeks of data to predict the next 13 weeks
window.ts <- window(weekly, start = c(1,length(weekly) - nTrain - nValid), end = c(1,length(weekly)))
nTest <- 13*7

test.lm.trend.season <- tslm(window.ts ~ trend + season)
test.lm.trend.season.forecast <- forecast(test.lm.trend.season, h = nTest)
plot(test.lm.trend.season.forecast)

plot(test.lm.trend.season.forecast,
     ylim = c(160,300 ),
     ylab = "Daily Price High",
     xlab = "Week",
     xlim = c(1131,1196),
     main = "Forecast for 3 months using Linear model with Trend and Sesonality")

text(1151, 290, "Training")
text(1176, 290, "Validation")
text(1190, 290,"Future")
lines(valid.ts, col="green")
lines(train.ts, col="red")

##For Low Price

#Creating time series - whole data set with different frequencies for low price
daily.l<- ts(data$Low, start = c(1), frequency = 1)
plot(daily.l)

weekly.l<- ts(data$Low, start = c(1), frequency = 7)
plot(weekly.l)
autoplot(decompose(weekly.l, type = "additive"))

yearly.l <- ts(data$Low, start = c(1), frequency = 365)
autoplot(yearly.l)
autoplot(decompose(yearly.l, type = "additive"))
acf(yearly.l)
pacf(yearly.l)

#Creating train & validation set for the latest year
train.ts.l <- window(weekly.l, start = c(1,length(weekly.l) - nTrain - nValid), end = c(1,length(weekly.l) - nValid))
valid.ts.l <- window(weekly.l, start = c(1,length(weekly.l) - nValid + 1))

#tslm model with trend and seasonality on validation set
train.lm.trend.season.l <- tslm(train.ts.l ~ trend + season)
train.lm.trend.season.forecast.l <- forecast(train.lm.trend.season.l, h = nValid)

plot(train.lm.trend.season.forecast.l,
     ylim = c(160,300 ),
     ylab = "Daily Price Low",
     xlab = "Week",
     xlim = c(1131,1183),
     main = "Predictive Model for 3 months using Linear model with Trend and Sesonality")
lines(valid.ts.l, col="green")
text(1151, 290, "Training")
text(1176, 290, "Validation")

#applying tslm(trend + season) model to 52 weeks of data to predict the next 13 weeks
window.ts.l <- window(weekly.l, start = c(1,length(weekly.l) - nTrain - nValid), end = c(1,length(weekly.l)))
nTest <- 13*7

test.lm.trend.season.l <- tslm(window.ts.l ~ trend + season)
test.lm.trend.season.forecast.l <- forecast(test.lm.trend.season.l, h = nTest)

plot(test.lm.trend.season.forecast.l,
     ylim = c(160,300 ),
     ylab = "Daily Price Low",
     xlab = "Week",
     xlim = c(1131,1196),
     main = "Forecast for 3 months using Linear model with Trend and Sesonality")

text(1151, 290, "Training")
text(1176, 290, "Validation")
text(1190, 290,"Future")
lines(valid.ts.l, col="green")
lines(train.ts.l, col="red")

#Optimal Price
max(test.lm.trend.season.forecast$mean)

##volume time series plot##
volume.ts <- ts(data$Volume, start=c(1), end=c(5746), freq=1)
plot(volume.ts)
yearly.v<- ts(data$Volume, start = c(1), frequency = 365)
decompose_coffee.v <- decompose(yearly.v, type = "additive")
plot(decompose_coffee.v)
